module Main where

import Control.Monad.State.Lazy (state, State)
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
   {-let result = runTrav initUserState trav-}

   {-(globals, state) <- checkResult "Semantic error" result-}
   {-let tups = userState state-}

   return ()

foo :: (MonadTrav m) => NodeInfo -> VarDecl -> CStat -> m DefTable
foo node_info decl s@(CCompound localLabels _ _) = do
   Trav.enterFunctionScope
   mapM_ (withDefTable . defineLabel) (localLabels ++ getLabels s)
   defineParams node_info decl -- record parameters
   dt <- getDefTable
   Trav.leaveFunctionScope
   return dt

funcTrace :: VarDecl -> Stmt -> NodeInfo -> String
funcTrace v s info = render $
   text "Definition: " <+> pretty v <+> text "\n" <+>
   text "Body: " <+> pretty s <+> text "\n" <+>
   text "Position: " <+> text (show $ posOfNode info) <+> text "\n"
   {-text "Node name: " <+> text (show $ nameOfNode info)-}

funcHandler :: DeclEvent -> Trav UserState ()
funcHandler (DeclEvent (FunctionDef fd@(FunDef v s info))) = do
   {-modifyUserState (\st -> -}
   {-freesParameter fd-}
   trace (funcTrace v s info) return ()
   return ()
   {-trace (funcTrace v s info) $ do-}
      {-dt <- getDefTable-}
      {-dt2 <- foo info v s-}
      {-modifyUserState (\fds -> (fd, dt, dt2):fds)-}
funcHandler _ = return ()

-- User state for Trav, consisting of the current function being traversed over,
-- as well as a map from function to parameters it has freed
type UserState = [FuncInfo]

type ScopedVariable = String
type Scope = [ScopedVariable]
data FuncInfo = FuncInfo { scopes      :: [Scope] -- list of scopes, where the tail has the parameters, and each new scope is a new element prepended to the list
                         , freedParams :: [String] -- list of parameters freed within the body of this function
                         }

-- setScopes info scopes sets info's scopes
setScopes :: FuncInfo -> [Scope] -> FuncInfo
setScopes info ss = info { scopes = ss }

-- modifyScopes info f applies f to info's scopes and returns the new info
modifyScopes :: FuncInfo -> ([Scope] -> [Scope]) -> FuncInfo
modifyScopes info f = setScopes info (f $ scopes info)

functionBodyEnterNewScope :: State FuncInfo ()
functionBodyEnterNewScope = state $ \info -> ((), modifyScopes info ([]:))

-- Leaves the current scope, returning it
functionBodyLeaveScope :: State FuncInfo Scope
functionBodyLeaveScope = state $
   \info -> let (s:_) = scopes info
            in (s, modifyScopes info tail)

-- Define a variable in the current scope of info
functionBodyDefineVar :: ScopedVariable -> State FuncInfo ()
functionBodyDefineVar var = state $
   \info -> let (s:ss) = scopes info
            in ((), setScopes info ((var:s):ss))

{-initUserState :: UserState-}
{-initUserState =-}
   {-UserState { -}
      {-curFunc     = undefined,-}
      {-freedParams = Map.empty-}
   {-}-}

{--- setCurFuncName funcName oldState updates oldState's curFuncName with funcName-}
{-setCurFuncName :: Ident -> UserState -> UserState-}
{-setCurFuncName newFunc oldState =-}
   {-UserState { -}
      {-curFunc     = newFunc,-}
      {-freedParams = freedParams oldState-}
   {-}-}

{-setFreedParams :: FuncMap -> UserState -> UserState-}
{-setFreedParams newFreedParams oldState =-}
   {-UserState { -}
      {-curFunc     = curFunc oldState,-}
      {-freedParams = newFreedParams-}

   {-}-}

{--- putFreedParam paramName oldState adds a freed parameter to the current-}
{--- function's list of freed parameters.-}
{----}
{--- Requires: current function name to exist in the map-}
{-putFreedParam :: Ident -> UserState -> UserState-}
{-putFreedParam paramName oldState =-}
   {-let-}
      {-fn      = curFunc oldState-}
      {-oldMap  = freedParams oldState-}
      {-newMap  = Map.adjust (paramName:) fn oldMap-}
   {-in-}
      {-setFreedParams newMap oldState-}

type MyTrav = Trav UserState ()

-- Statement handlers

handleStmt :: CStatement a -> State FuncInfo ()
handleStmt (CLabel ident st attrs at)        = handleLabelStmt ident st attrs at
handleStmt (CCase ex st at)                  = handleCaseStmt ex st at
handleStmt (CCases ex1 ex2 st at)            = handleCasesStmt ex1 ex2 st at
handleStmt (CDefault st at)                  = handleDefaultStmt st at
handleStmt (CExpr ex at)                     = handleExprStmt ex at
handleStmt (CCompound idents block_items at) = handleCompoundStmt idents block_items at
handleStmt (CIf ex st1 st2 at)               = handleIfStmt ex st1 st2 at
handleStmt (CSwitch ex st at)                = handleSwitchStmt ex st at
handleStmt (CWhile ex st isDoWhile at)       = handleWhileStmt ex st isDoWhile at
handleStmt (CFor init ex2 ex3 st at)         = handleForStmt init ex2 ex3 st at
handleStmt (CGoto ident at)                  = handleGotoStmt ident at
handleStmt (CGotoPtr ex at)                  = handleGotoPtrStmt ex at
handleStmt (CCont at)                        = handleContStmt at
handleStmt (CBreak at)                       = handleBreakStmt at
handleStmt (CReturn ex at)                   = handleReturnStmt ex at
handleStmt (CAsm st at)                      = handleAsmStmt st at

handleLabelStmt :: Ident -> CStatement a -> [CAttribute a] -> a -> State FuncInfo ()
handleLabelStmt _ st _ _ = handleStmt st

handleCaseStmt :: CExpression a -> CStatement a -> a -> State FuncInfo ()
handleCaseStmt ex st _ = do
   handleExpr ex
   handleStmt st

handleCasesStmt :: CExpression a -> CExpression a -> CStatement a -> a -> State FuncInfo ()
handleCasesStmt ex1 ex2 st _ = do
   handleExpr ex1
   handleExpr ex2
   handleStmt st

handleDefaultStmt :: CStatement a -> a -> State FuncInfo ()
handleDefaultStmt st _ = handleStmt st

handleExprStmt :: Maybe (CExpression a) -> a -> State FuncInfo ()
handleExprStmt Nothing _   = return ()
handleExprStmt (Just ex) _ = handleExpr ex

handleCompoundStmt :: [Ident] -> [CCompoundBlockItem a] -> a -> State FuncInfo ()
handleCompoundStmt _ block_items _ = do
   functionBodyEnterNewScope
   mapM_ handleCompoundBlockItem block_items
   _ <- functionBodyLeaveScope
   return ()

handleCompoundBlockItem :: CCompoundBlockItem a -> State FuncInfo ()
handleCompoundBlockItem (CBlockStmt st)      = handleStmt st
handleCompoundBlockItem (CBlockDecl decl)    = handleDeclaration decl
handleCompoundBlockItem (CNestedFunDef fdef) = handleNestedFunDef fdef

data DeclarationType = ToplevelDecl | StructDecl | ParamDecl | TypenameDecl
declarationType :: CDeclaration a -> DeclarationType
declarationType (CDecl _ declrs _) = ToplevelDecl -- TODO: Flush this out

handleDeclaration :: CDeclaration a -> State FuncInfo ()
handleDeclaration decl@(CDecl specs init_declr_list node_info) =
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

handleNestedFunDef :: CFunctionDef a -> State FuncInfo ()
handleNestedFunDef = undefined -- TODO

handleIfStmt :: CExpression a -> CStatement a -> Maybe (CStatement a) ->
                     a -> State FuncInfo ()
handleIfStmt ex st1 st2 _ = do
   handleExpr ex
   handleStmt st1
   forM_ st2 handleStmt

handleSwitchStmt :: CExpression a -> CStatement a -> a -> State FuncInfo ()
handleSwitchStmt ex st _ = do
   handleExpr ex
   handleStmt st

handleWhileStmt :: CExpression a -> CStatement a -> Bool -> a -> State FuncInfo ()
handleWhileStmt ex st _ _ = do
   handleExpr ex
   handleStmt st

handleForStmt :: Either (Maybe (CExpression a)) (CDeclaration a) ->
                      Maybe (CExpression a) -> Maybe (CExpression a) ->
                      CStatement a -> a -> State FuncInfo ()
handleForStmt init ex2 ex3 st _ = do
   case init of
      Left Nothing    -> return ()
      Left (Just ex1) -> handleExpr ex1
      Right decl      -> handleDeclaration decl
   forM_ ex2 handleExpr
   forM_ ex3 handleExpr
   handleStmt st

handleGotoStmt :: Ident -> a -> State FuncInfo ()
handleGotoStmt _ _ = return ()

handleGotoPtrStmt :: CExpression a -> a -> State FuncInfo ()
handleGotoPtrStmt ex _ = handleExpr ex

handleContStmt :: a -> State FuncInfo ()
handleContStmt _ = return ()

handleBreakStmt :: a -> State FuncInfo ()
handleBreakStmt _ = return ()

handleReturnStmt :: Maybe (CExpression a) -> a -> State FuncInfo ()
handleReturnStmt ex _ = forM_ ex handleExpr

handleAsmStmt :: CAssemblyStatement a -> a -> State FuncInfo ()
handleAsmStmt = undefined

-- Expression handlers

handleExpr :: CExpression a -> State FuncInfo ()
handleExpr (CComma exs at)                  = handleCommaExpr exs at
handleExpr (CAssign op lex rex at)          = handleAssignExpr op lex rex at
handleExpr (CCond cond tex fex at)          = handleCondExpr cond tex fex at
handleExpr (CBinary op lex rex at)          = handleBinaryExpr op lex rex at
handleExpr (CCast typename ex at)           = handleCastExpr typename ex at
handleExpr (CUnary op ex at)                = handleUnaryExpr op ex at
handleExpr (CSizeofExpr ex at)              = handleSizeofExprExpr ex at
handleExpr (CSizeofType typename at)        = handleSizeofTypeExpr typename at
handleExpr (CAlignofExpr ex at)             = handleAlignofExprExpr ex at
handleExpr (CAlignofType typename at)       = handleAlignofTypeExpr typename at
handleExpr (CComplexReal real at)           = handleComplexRealExpr real at
handleExpr (CComplexImag imag at)           = handleComplexImagExpr imag at
handleExpr (CIndex array index at)          = handleIndexExpr array index at
handleExpr (CCall func args at)             = handleCallExpr func args at
handleExpr (CMember struct member deref at) = handleMemberExpr struct member deref at
handleExpr (CVar ident at)                  = handleVarExpr ident at
handleExpr (CConst const)                   = handleConstExpr const
handleExpr (CCompoundLit typename inits at) = handleCompoundLitExpr typename inits at
handleExpr (CStatExpr st at)                = handleStmtExpr st at
handleExpr (CLabAddrExpr label at)          = handleLabAddrExpr label at
handleExpr (CBuiltinExpr builtin)           = handleBuiltinExpr builtin

handleCommaExpr :: [CExpression a] -> a -> State FuncInfo ()
handleCommaExpr exs _ = mapM_ handleExpr exs

handleAssignExpr :: CAssignOp -> CExpression a -> CExpression a -> a -> State FuncInfo ()
handleAssignExpr _ lex rex _ = do
   handleExpr lex
   handleExpr rex

handleCondExpr :: CExpression a -> Maybe (CExpression a) -> CExpression a ->
                  a -> State FuncInfo ()
handleCondExpr cond tex fex _ = do
   handleExpr cond
   forM_ tex handleExpr
   handleExpr fex

handleBinaryExpr :: CBinaryOp -> CExpression a -> CExpression a -> a -> State FuncInfo ()
handleBinaryExpr _ lex rex _ = do
   handleExpr lex
   handleExpr rex

handleCastExpr :: CDeclaration a -> CExpression a -> a -> State FuncInfo ()
handleCastExpr _ ex _ = handleExpr ex

handleUnaryExpr :: CUnaryOp -> CExpression a -> a -> State FuncInfo ()
handleUnaryExpr _ ex _ = handleExpr ex

handleSizeofExprExpr :: CExpression a -> a -> State FuncInfo ()
handleSizeofExprExpr ex _ = handleExpr ex

handleSizeofTypeExpr :: CDeclaration a -> a -> State FuncInfo ()
handleSizeofTypeExpr _ _ = return ()

handleAlignofExprExpr :: CExpression a -> a -> State FuncInfo ()
handleAlignofExprExpr ex _ = handleExpr ex

handleAlignofTypeExpr :: CDeclaration a -> a -> State FuncInfo ()
handleAlignofTypeExpr _ _ = return ()

handleComplexRealExpr :: CExpression a -> a -> State FuncInfo ()
handleComplexRealExpr real _ = handleExpr real

handleComplexImagExpr :: CExpression a -> a -> State FuncInfo ()
handleComplexImagExpr imag _ = handleExpr imag

handleIndexExpr :: CExpression a -> CExpression a -> a -> State FuncInfo ()
handleIndexExpr array index _ = do
   handleExpr array
   handleExpr index

handleCallExpr :: CExpression a -> [CExpression a] -> a -> State FuncInfo ()
handleCallExpr = undefined -- TODO
{-handleCallExpr func args _ = do-}
   {-case func of-}
      {-CVar (name, n, info) ->-}
         {-if name == "free"-}
         {-then modifyUserState (\m -> if notMember-}

handleMemberExpr :: CExpression a -> Ident -> Bool -> a -> State FuncInfo ()
handleMemberExpr struct member deref at = handleExpr struct

handleVarExpr :: Ident -> a -> State FuncInfo ()
handleVarExpr ident at = return ()

handleConstExpr :: CConstant a -> State FuncInfo ()
handleConstExpr const = return ()

handleCompoundLitExpr :: CDeclaration a -> CInitializerList a -> a ->
                         State FuncInfo ()
handleCompoundLitExpr typename inits at = handleDeclaration typename -- TODO: correct?

handleStmtExpr :: CStatement a -> a -> State FuncInfo ()
handleStmtExpr st at = handleStmt st

handleLabAddrExpr :: Ident -> a -> State FuncInfo ()
handleLabAddrExpr label at = return ()

handleBuiltinExpr :: CBuiltinThing a -> State FuncInfo ()
handleBuiltinExpr = undefined -- TODO

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

