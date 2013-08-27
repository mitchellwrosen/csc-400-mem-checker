module FunctionBodyTravMonad where

import Language.C.Syntax.AST

class Monad m => MonadFunctionBodyTrav m where
   handleStmt :: CStatement a -> m ()
