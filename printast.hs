module Foo where

import Language.C
import Language.C.System.GCC

checkResult :: (Show a) => String -> Either a b -> IO b
checkResult label = either (error . (label++) . show) return

printAST :: String -> IO ()
printAST filename = do
   ast <- parseCFile (newGCC "gcc") Nothing [] filename >>= checkResult "parsing"
   print $ ast
