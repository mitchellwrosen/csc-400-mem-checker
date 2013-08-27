module Main where

import FunctionBodyTravMonad

import Control.Monad.State.Lazy (execState, state, State)
import Data.Foldable (forM_)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Debug.Trace (trace)
import Text.PrettyPrint.HughesPJ ((<+>), cat, Doc, render, text)

import Language.C                 -- Language.C.{Syntax,Pretty,Parser,InputStream}
import Language.C.Analysis.AstAnalysis
import Language.C.Analysis.Builtins
import Language.C.Analysis.ConstEval
import Language.C.Analysis.Debug
import Language.C.Analysis.DeclAnalysis
import Language.C.Analysis.DefTable
import Language.C.Analysis.Export
import Language.C.Analysis.NameSpaceMap
import Language.C.Analysis.SemError
import Language.C.Analysis.SemRep
import Language.C.Analysis.TravMonad as Trav
import Language.C.Analysis.TypeCheck
import Language.C.Analysis.TypeConversions
import Language.C.Analysis.TypeUtils
import Language.C.Data.Error
import Language.C.Data.Ident
import Language.C.Data.InputStream
import Language.C.Data.Name
import Language.C.Data.Node
import Language.C.Data.Position
import Language.C.Syntax.AST
import Language.C.Syntax.Constants
import Language.C.Syntax.Ops
import Language.C.Syntax.Utils
import Language.C.System.GCC
import Language.C.System.Preprocess

parseTestC :: IO CTranslUnit
parseTestC = do
   result <- parseCFile (newGCC "gcc") Nothing [] "test.c"
   checkResult "Parse error" result

printMyAST :: CTranslUnit -> IO ()
printMyAST = print . pretty

main :: IO ()
main = do
   ast <- parseTestC

   let trav = withExtDeclHandler (analyseAST ast) funcHandler
   let result = runTrav initFuncInfoMap trav

   (globals, state) <- checkResult "Semantic error" result
   let tups = userState state

   return ()

funcTrace :: VarDecl -> Stmt -> NodeInfo -> String
funcTrace v s info = render $
   text "Definition: " <+> pretty v <+> text "\n" <+>
   text "Body: " <+> pretty s <+> text "\n" <+>
   text "Position: " <+> text (show $ posOfNode info) <+> text "\n"
   {-text "Node name: " <+> text (show $ nameOfNode info)-}

funcHandler :: DeclEvent -> Trav FuncInfoMap ()
funcHandler (DeclEvent (FunctionDef (FunDef v s info))) = do
   trace (funcTrace v s info) return ()

{-params (FunDef (VarDecl _ _ (FunctionType (FunType _ ps _) _)) _ _) = ps-}
   let (VarDecl (VarName func_ident _) _ (FunctionType (FunType _ ps _) _)) = v
   let func_name = identToString func_ident
   let param_decls = ps -- TODO: get idents

   {-let func_info = unFBTrav $-}
   let func_info = execState (handleStmt s) initFuncInfo

   trace func_name (return ())
   trace (show func_info) (return ())

   modifyUserState (Map.insert func_name func_info)
funcHandler _ = return ()

-- User state for Trav, consisting of the current function being traversed over,
-- as well as a map from function to parameters it has freed
type FuncInfoMap = Map.Map String FuncInfo
initFuncInfoMap :: FuncInfoMap
initFuncInfoMap = Map.empty

type MyTrav = Trav FuncInfoMap ()

--------------------------------------------------------------------------------

newtype FunctionBodyTrav s a = FunctionBodyTrav { unTrav :: State s a }

instance Monad (FunctionBodyTrav s) where
   return x = FunctionBodyTrav $ return x
   (FunctionBodyTrav h) >>= f = FunctionBodyTrav $ h >>= (unTrav . f)

instance MonadFunctionBodyTrav (FunctionBodyTrav s) where
   handleStmt          = doHandleStmt

type ScopedVariable = String
type Scope = [ScopedVariable]

-- User state for FunctionBodyTrav
data FuncInfo = FuncInfo {
   scopes      :: [Scope], -- list of scopes, where the tail has the parameters, and each new scope is a new element prepended to the list
   freedParams :: [String] -- list of parameters freed within the body of this function
} deriving (Show)

-- Initial function info, given the parameters
{-initFuncInfo :: VarDecl -> FuncInfo -- TODO-}
initFuncInfo :: FuncInfo
initFuncInfo = FuncInfo {
   scopes      = [],
   freedParams = []
}

-- setScopes info scopes sets info's scopes
setScopes :: FuncInfo -> [Scope] -> FuncInfo
setScopes info ss = info { scopes = ss }

-- modifyScopes info f applies f to info's scopes and returns the new info
modifyScopes :: FuncInfo -> ([Scope] -> [Scope]) -> FuncInfo
modifyScopes info f = setScopes info (f $ scopes info)

-- Define a variable in the current scope of info
functionBodyDefineVar :: MonadFunctionBodyTrav m => ScopedVariable -> m ()
functionBodyDefineVar var =
   trace ("Defining " ++ var) $
   state $
   \info -> let (s:ss) = scopes info
            in ((), setScopes info ((var:s):ss))

bodyEnterBlockScope :: MonadFunctionBodyTrav m => m ()
bodyEnterBlockScope = state $ \info -> ((), modifyScopes info ([]:))

-- Leaves the current scope, returning it
bodyLeaveBlockScope :: MonadFunctionBodyTrav m => m Scope
bodyLeaveBlockScope = state $
   \info -> let (s:_) = scopes info
            in (s, modifyScopes info tail)



-- Statement handlers

doHandleStmt :: MonadFunctionBodyTrav m => CStatement a -> m ()
doHandleStmt (CLabel ident st attrs at)        = doHandleLabelStmt ident st attrs at
doHandleStmt (CCase ex st at)                  = doHandleCaseStmt ex st at
doHandleStmt (CCases ex1 ex2 st at)            = doHandleCasesStmt ex1 ex2 st at
doHandleStmt (CDefault st at)                  = doHandleDefaultStmt st at
doHandleStmt (CExpr ex at)                     = doHandleExprStmt ex at
doHandleStmt (CCompound idents block_items at) = doHandleCompoundStmt idents block_items at
doHandleStmt (CIf ex st1 st2 at)               = doHandleIfStmt ex st1 st2 at
doHandleStmt (CSwitch ex st at)                = doHandleSwitchStmt ex st at
doHandleStmt (CWhile ex st isDoWhile at)       = doHandleWhileStmt ex st isDoWhile at
doHandleStmt (CFor init ex2 ex3 st at)         = doHandleForStmt init ex2 ex3 st at
doHandleStmt (CGoto ident at)                  = doHandleGotoStmt ident at
doHandleStmt (CGotoPtr ex at)                  = doHandleGotoPtrStmt ex at
doHandleStmt (CCont at)                        = doHandleContStmt at
doHandleStmt (CBreak at)                       = doHandleBreakStmt at
doHandleStmt (CReturn ex at)                   = doHandleReturnStmt ex at
doHandleStmt (CAsm st at)                      = doHandleAsmStmt st at

doHandleLabelStmt :: MonadFunctionBodyTrav m => Ident -> CStatement a -> [CAttribute a] -> a -> m ()
doHandleLabelStmt _ st _ _ = doHandleStmt st

doHandleCaseStmt :: MonadFunctionBodyTrav m => CExpression a -> CStatement a -> a -> m ()
doHandleCaseStmt ex st _ = do
   doHandleExpr ex
   doHandleStmt st

doHandleCasesStmt :: MonadFunctionBodyTrav m => CExpression a ->
                     CExpression a -> CStatement a -> a -> m ()
doHandleCasesStmt ex1 ex2 st _ = do
   doHandleExpr ex1
   doHandleExpr ex2
   doHandleStmt st

doHandleDefaultStmt :: MonadFunctionBodyTrav m => CStatement a -> a -> m ()
doHandleDefaultStmt st _ = doHandleStmt st

doHandleExprStmt :: MonadFunctionBodyTrav m => Maybe (CExpression a) -> a -> m ()
doHandleExprStmt Nothing _   = return ()
doHandleExprStmt (Just ex) _ = doHandleExpr ex

doHandleCompoundStmt :: MonadFunctionBodyTrav m => [Ident] -> [CCompoundBlockItem a] -> a -> m ()
doHandleCompoundStmt _ block_items _ = do
   bodyEnterBlockScope
   mapM_ doHandleCompoundBlockItem block_items
   _ <- bodyLeaveBlockScope
   return ()

doHandleCompoundBlockItem :: MonadFunctionBodyTrav m => CCompoundBlockItem a -> m ()
doHandleCompoundBlockItem (CBlockStmt st)      = doHandleStmt st
doHandleCompoundBlockItem (CBlockDecl decl)    = doHandleDeclaration decl
doHandleCompoundBlockItem (CNestedFunDef fdef) = doHandleNestedFunDef fdef

data DeclarationType = ToplevelDecl | StructDecl | ParamDecl | TypenameDecl
declarationType :: CDeclaration a -> DeclarationType
declarationType (CDecl _ declrs _) = ToplevelDecl -- TODO: Flush this out

doHandleDeclaration :: MonadFunctionBodyTrav m => CDeclaration a -> m ()
doHandleDeclaration decl@(CDecl specs init_declr_list node_info) =
   case declarationType decl of
      ToplevelDecl -> do
         let declrs   = getDeclrs init_declr_list
         let idents   = mapMaybe getIdent declrs
         let varnames = map identToString idents
         forM_ varnames functionBodyDefineVar
         return ()
      _ -> return ()
   where getDeclrs = map (\d -> let (Just declr, _, _) = d in declr)

         getIdent :: CDeclarator a -> Maybe Ident
         getIdent (CDeclr ident _ _ _ _) = ident

doHandleNestedFunDef :: MonadFunctionBodyTrav m => CFunctionDef a -> m ()
doHandleNestedFunDef = undefined -- TODO

doHandleIfStmt :: MonadFunctionBodyTrav m => CExpression a -> CStatement a -> Maybe (CStatement a) -> a -> m ()
doHandleIfStmt ex st1 st2 _ = do
   doHandleExpr ex
   doHandleStmt st1
   forM_ st2 doHandleStmt

doHandleSwitchStmt :: MonadFunctionBodyTrav m => CExpression a -> CStatement a -> a -> m ()
doHandleSwitchStmt ex st _ = do
   doHandleExpr ex
   doHandleStmt st

doHandleWhileStmt :: MonadFunctionBodyTrav m => CExpression a -> CStatement a -> Bool -> a -> m ()
doHandleWhileStmt ex st _ _ = do
   doHandleExpr ex
   doHandleStmt st

doHandleForStmt :: MonadFunctionBodyTrav m =>
                   Either (Maybe (CExpression a)) (CDeclaration a) ->
                   Maybe (CExpression a) -> Maybe (CExpression a) ->
                   CStatement a -> a -> m ()
doHandleForStmt init ex2 ex3 st _ = do
   case init of
      Left Nothing    -> return ()
      Left (Just ex1) -> doHandleExpr ex1
      Right decl      -> doHandleDeclaration decl
   forM_ ex2 doHandleExpr
   forM_ ex3 doHandleExpr
   doHandleStmt st

doHandleGotoStmt :: MonadFunctionBodyTrav m => Ident -> a -> m ()
doHandleGotoStmt _ _ = return ()

doHandleGotoPtrStmt :: MonadFunctionBodyTrav m => CExpression a -> a -> m ()
doHandleGotoPtrStmt ex _ = doHandleExpr ex

doHandleContStmt :: MonadFunctionBodyTrav m => a -> m ()
doHandleContStmt _ = return ()

doHandleBreakStmt :: MonadFunctionBodyTrav m => a -> m ()
doHandleBreakStmt _ = return ()

doHandleReturnStmt :: MonadFunctionBodyTrav m => Maybe (CExpression a) -> a -> m ()
doHandleReturnStmt ex _ = forM_ ex doHandleExpr

doHandleAsmStmt :: MonadFunctionBodyTrav m => CAssemblyStatement a -> a -> m ()
doHandleAsmStmt = undefined

-- Expression doHandlers

doHandleExpr :: MonadFunctionBodyTrav m => CExpression a -> m ()
doHandleExpr (CComma exs at)                  = doHandleCommaExpr exs at
doHandleExpr (CAssign op lex rex at)          = doHandleAssignExpr op lex rex at
doHandleExpr (CCond cond tex fex at)          = doHandleCondExpr cond tex fex at
doHandleExpr (CBinary op lex rex at)          = doHandleBinaryExpr op lex rex at
doHandleExpr (CCast typename ex at)           = doHandleCastExpr typename ex at
doHandleExpr (CUnary op ex at)                = doHandleUnaryExpr op ex at
doHandleExpr (CSizeofExpr ex at)              = doHandleSizeofExprExpr ex at
doHandleExpr (CSizeofType typename at)        = doHandleSizeofTypeExpr typename at
doHandleExpr (CAlignofExpr ex at)             = doHandleAlignofExprExpr ex at
doHandleExpr (CAlignofType typename at)       = doHandleAlignofTypeExpr typename at
doHandleExpr (CComplexReal real at)           = doHandleComplexRealExpr real at
doHandleExpr (CComplexImag imag at)           = doHandleComplexImagExpr imag at
doHandleExpr (CIndex array index at)          = doHandleIndexExpr array index at
doHandleExpr (CCall func args at)             = doHandleCallExpr func args at
doHandleExpr (CMember struct member deref at) = doHandleMemberExpr struct member deref at
doHandleExpr (CVar ident at)                  = doHandleVarExpr ident at
doHandleExpr (CConst const)                   = doHandleConstExpr const
doHandleExpr (CCompoundLit typename inits at) = doHandleCompoundLitExpr typename inits at
doHandleExpr (CStatExpr st at)                = doHandleStmtExpr st at
doHandleExpr (CLabAddrExpr label at)          = doHandleLabAddrExpr label at
doHandleExpr (CBuiltinExpr builtin)           = doHandleBuiltinExpr builtin

doHandleCommaExpr :: MonadFunctionBodyTrav m => [CExpression a] -> a -> m ()
doHandleCommaExpr exs _ = mapM_ doHandleExpr exs

doHandleAssignExpr :: MonadFunctionBodyTrav m => CAssignOp -> CExpression a -> CExpression a -> a -> m ()
doHandleAssignExpr _ lex rex _ = do
   doHandleExpr lex
   doHandleExpr rex

doHandleCondExpr :: MonadFunctionBodyTrav m => CExpression a -> Maybe (CExpression a) -> CExpression a -> a -> m ()
doHandleCondExpr cond tex fex _ = do
   doHandleExpr cond
   forM_ tex doHandleExpr
   doHandleExpr fex

doHandleBinaryExpr :: MonadFunctionBodyTrav m => CBinaryOp -> CExpression a -> CExpression a -> a -> m ()
doHandleBinaryExpr _ lex rex _ = do
   doHandleExpr lex
   doHandleExpr rex

doHandleCastExpr :: MonadFunctionBodyTrav m => CDeclaration a -> CExpression a -> a -> m ()
doHandleCastExpr _ ex _ = doHandleExpr ex

doHandleUnaryExpr :: MonadFunctionBodyTrav m => CUnaryOp -> CExpression a -> a -> m ()
doHandleUnaryExpr _ ex _ = doHandleExpr ex

doHandleSizeofExprExpr :: MonadFunctionBodyTrav m => CExpression a -> a -> m ()
doHandleSizeofExprExpr ex _ = doHandleExpr ex

doHandleSizeofTypeExpr :: MonadFunctionBodyTrav m => CDeclaration a -> a -> m ()
doHandleSizeofTypeExpr _ _ = return ()

doHandleAlignofExprExpr :: MonadFunctionBodyTrav m => CExpression a -> a -> m ()
doHandleAlignofExprExpr ex _ = doHandleExpr ex

doHandleAlignofTypeExpr :: MonadFunctionBodyTrav m => CDeclaration a -> a -> m ()
doHandleAlignofTypeExpr _ _ = return ()

doHandleComplexRealExpr :: MonadFunctionBodyTrav m => CExpression a -> a -> m ()
doHandleComplexRealExpr real _ = doHandleExpr real

doHandleComplexImagExpr :: MonadFunctionBodyTrav m => CExpression a -> a -> m ()
doHandleComplexImagExpr imag _ = doHandleExpr imag

doHandleIndexExpr :: MonadFunctionBodyTrav m => CExpression a -> CExpression a -> a -> m ()
doHandleIndexExpr array index _ = do
   doHandleExpr array
   doHandleExpr index

doHandleCallExpr :: MonadFunctionBodyTrav m => CExpression a -> [CExpression a] -> a -> m ()
doHandleCallExpr = undefined -- TODO
{-doHandleCallExpr func args _ = do-}
   {-case func of-}
      {-CVar (name, n, info) ->-}
         {-if name == "free"-}
         {-then modifyUserState (\m -> if notMember-}

doHandleMemberExpr :: MonadFunctionBodyTrav m => CExpression a -> Ident -> Bool -> a -> m ()
doHandleMemberExpr struct member deref at = doHandleExpr struct

doHandleVarExpr :: MonadFunctionBodyTrav m => Ident -> a -> m ()
doHandleVarExpr ident at = return ()

doHandleConstExpr :: MonadFunctionBodyTrav m => CConstant a -> m ()
doHandleConstExpr const = return ()

doHandleCompoundLitExpr :: MonadFunctionBodyTrav m => CDeclaration a -> CInitializerList a -> a -> m ()
doHandleCompoundLitExpr typename inits at = doHandleDeclaration typename -- TODO: correct?

doHandleStmtExpr :: MonadFunctionBodyTrav m => CStatement a -> a -> m ()
doHandleStmtExpr st at = doHandleStmt st

doHandleLabAddrExpr :: MonadFunctionBodyTrav m => Ident -> a -> m ()
doHandleLabAddrExpr label at = return ()

doHandleBuiltinExpr :: MonadFunctionBodyTrav m => CBuiltinThing a -> m ()
doHandleBuiltinExpr = undefined -- TODO

freesParameter :: MonadTrav m => FunDef -> m [Int]
freesParameter fd@(FunDef var_decl stat@(CCompound localLabels blockItems _) node_info) = do
   Trav.enterFunctionScope
   mapM_ (withDefTable . defineLabel) (localLabels ++ getLabels stat)

   let params = funParams fd
   let ident = funIdent fd
   trace (render $ pretty ident
               <+> text "params: "
               <+> cat (map pretty params))
         (return ())

   {-freed <- mapM getFreed_cbi blockItems-}

   {-dt <- getDefTable-}
   Trav.leaveFunctionScope
   return []

-- Layer 1 - properties of function declaration
-- Identifier of the variable declaration
funIdent :: FunDef -> Ident
funIdent = declIdent . getVarDecl

-- Attributes of the variable declaration
funAttributes :: FunDef -> DeclAttrs
funAttributes = declAttrs . getVarDecl

-- Layer 2 - properties of the function declaration's variable declaration
funType :: FunDef -> FunType
funType fd = let (FunctionType ft _) = (declType . getVarDecl) fd in ft

funTypeAttributes :: FunDef -> Attributes
funTypeAttributes fd =
   let (FunctionType _ attrs) = (declType . getVarDecl) fd in attrs

-- Gets the return type of function
funReturnType :: FunDef -> Type
funReturnType fd = case funType fd of
                      (FunType t _ _)       -> t
                      (FunTypeIncomplete t) -> t

-- Gets the parameters of a function
funParams :: FunDef -> [ParamDecl]
funParams fd = fromMaybe (error "incomplete function definition")
                         (getParams' fd)
   where getParams' :: FunDef -> Maybe [ParamDecl]
         getParams' fd = case funType fd of
                            (FunType _ params _) -> Just params
                            _                    -> Nothing

-- Gets whether the function has a variadic argument or not
funIsVariadic :: FunDef -> Bool
funIsVariadic fd = fromMaybe (error "incomplete function definition")
                             (getIsVariadic' fd)
   where getIsVariadic' :: FunDef -> Maybe Bool
         getIsVariadic' fd = case funType fd of
                                (FunType _ _ b) -> Just b
                                _               -> Nothing

checkResult :: (Show a) => String -> Either a b -> IO b
checkResult label = either (error . (label++) . show) return

{-frees :: FunDef -> [-}
{-params :: FunDef -> [ParamDecl]-}
{-params (FunDef (VarDecl _ _ (FunctionType (FunType _ ps _) _)) _ _) = ps-}
{-params _ = Nothing-}

declTrace :: DeclEvent -> String
declTrace event =
   render $ case event of
      TagEvent tag_def      -> text "Tag:" <+> pretty tag_def <+> file tag_def
      DeclEvent ident_decl  -> text "Decl:" <+> pretty ident_decl <+> file ident_decl
      ParamEvent pd         -> text "Param:" <+> pretty pd  <+> file pd
      LocalEvent ident_decl -> text "Local:" <+> pretty ident_decl  <+> file ident_decl
      TypeDefEvent tydef    -> text "Typedef:" <+> pretty tydef <+> file tydef
      AsmEvent _            -> text "Assembler block"
   where
      file :: (CNode a) => a -> Doc
      file = text . show . posOfNode . nodeInfo

debugHandler :: DeclEvent -> Trav s ()
debugHandler e = trace (declTrace e) (return ())

