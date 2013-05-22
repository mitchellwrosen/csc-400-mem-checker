module Parser where

import Data.Char
import Data.List

data Token = TK_EOF
           | TK_AUTO
           | TK_BREAK
           | TK_CASE
           | TK_CHAR
           | TK_CONST
           | TK_CONTINUE
           | TK_DEFAULT
           | TK_DO
           | TK_DOUBLE
           | TK_ELSE
           | TK_ENUM
           | TK_EXTERN
           | TK_FLOAT
           | TK_FOR
           | TK_GOTO
           | TK_IF
           | TK_INLINE
           | TK_INT
           | TK_LONG
           | TK_REGISTER
           | TK_RESTRICT
           | TK_RETURN
           | TK_SHORT
           | TK_SIGNED
           | TK_SIZEOF
           | TK_STATIC
           | TK_STRUCT
           | TK_SWITCH
           | TK_TYPEDEF
           | TK_UNION
           | TK_UNSIGNED
           | TK_VOID
           | TK_VOLATILE
           | TK_WHILE

           | TK_IDENTIFIER String
           | TK_CONSTANT String
           | TK_STRING_LITERAL String

           | TK_ELLIPSIS
           | TK_RIGHT_ASSIGN
           | TK_LEFT_ASSIGN
           | TK_ADD_ASSIGN
           | TK_SUB_ASSIGN
           | TK_MUL_ASSIGN
           | TK_DIV_ASSIGN
           | TK_MOD_ASSIGN
           | TK_AND_ASSIGN
           | TK_XOR_ASSIGN
           | TK_OR_ASSIGN
           | TK_RIGHT_OP
           | TK_LEFT_OP
           | TK_INC_OP
           | TK_DEC_OP
           | TK_PTR_OP
           | TK_AND_OP
           | TK_OR_OP
           | TK_LE_OP
           | TK_GE_OP
           | TK_EQ_OP
           | TK_NE_OP
           | TK_SEMICOLON
           | TK_LEFT_CURLY_BRACKET
           | TK_RIGHT_CURLY_BRACKET
           | TK_COMMA
           | TK_COLON
           | TK_ASSIGN
           | TK_LEFT_PAREN
           | TK_RIGHT_PAREN
           | TK_LEFT_SQUARE_BRACKET
           | TK_RIGHT_SQUARE_BRACKET
           | TK_DOT
           | TK_AMP
           | TK_NOT
           | TK_TILDE
           | TK_MINUS
           | TK_PLUS
           | TK_TIMES
           | TK_DIV
           | TK_MOD
           | TK_LT_OP
           | TK_GT_OP
           | TK_XOR
           | TK_PIPE
           | TK_QUESTION_MARK
           deriving Show

-- Helper functions. L = [a-zA-Z_]
isL :: Char -> Bool
isL x = isAlpha x || x == '_'

isLD :: Char -> Bool
isLD x = isL x || isDigit x

-- Lex a file
clexFile :: FilePath -> IO [Token]
clexFile f = do
   contents <- readFile f
   return $ clex contents

-- Lex a string
clex :: String -> [Token]
clex "" = []
clex xs = fst pair : clex (snd pair)
   where pair = next xs

-- Given a string, return the token at the head + the rest of the string
next :: String -> (Token, String)
next [] = (TK_EOF, "")
next xs@(x:xt)
   | isSpace x            = next xt
   | "/*" `isPrefixOf` xs = next (consumeBlockComment $ drop 2 xs)
   | "//" `isPrefixOf` xs = next (consumeCppStyleComment $ drop 2 xs)
   | isL x                = getIdentifier xs
   | isDigit x            = getConstant xs
   | x == '"'             = getStringLiteral xt
   | x == '.' =
      if ".." `isPrefixOf` xt then (TK_ELLIPSIS, drop 3 xs)
      else (TK_DOT, xt)
   | x == '>' =
      if ">=" `isPrefixOf` xt then (TK_RIGHT_ASSIGN, drop 3 xs)
      else if ">" `isPrefixOf` xt then (TK_RIGHT_OP, drop 2 xs)
      else if "=" `isPrefixOf` xt then (TK_GE_OP, drop 2 xs)
      else (TK_GT_OP, xt)
   | x == '<' =
      if "<=" `isPrefixOf` xt then (TK_LEFT_ASSIGN, drop 3 xs)
      else if "<" `isPrefixOf` xt then (TK_LEFT_OP, drop 2 xs)
      else if "=" `isPrefixOf` xt then (TK_LE_OP, drop 2 xs)
      else if "%" `isPrefixOf` xt then (TK_LEFT_CURLY_BRACKET, drop 2 xs)
      else if ":" `isPrefixOf` xt then (TK_LEFT_SQUARE_BRACKET, drop 2 xs)
      else (TK_LT_OP, xt)
   | x == '+' =
      if "=" `isPrefixOf` xt then (TK_ADD_ASSIGN, drop 2 xs)
      else if "+" `isPrefixOf` xt then (TK_INC_OP, drop 2 xs)
      else (TK_PLUS, xt)
   | x == '-' =
      if "=" `isPrefixOf` xt then (TK_SUB_ASSIGN, drop 2 xs)
      else if "-" `isPrefixOf` xt then (TK_DEC_OP, drop 2 xs)
      else if ">" `isPrefixOf` xt then (TK_PTR_OP, drop 2 xs)
      else (TK_MINUS, xt)
   | x == '*' =
      if "=" `isPrefixOf` xt then (TK_MUL_ASSIGN, drop 2 xs)
      else (TK_TIMES, xt)
   | x == '/' =
      if "=" `isPrefixOf` xt then (TK_DIV_ASSIGN, drop 2 xs)
      else (TK_DIV, xt)
   | x == '%' =
      if "=" `isPrefixOf` xt then (TK_MOD_ASSIGN, drop 2 xs)
      else if ">" `isPrefixOf` xt then (TK_RIGHT_CURLY_BRACKET, drop 2 xs)
      else (TK_MOD, xt)
   | x == '&' =
      if "=" `isPrefixOf` xt then (TK_AND_ASSIGN, drop 2 xs)
      else if "&" `isPrefixOf` xt then (TK_AND_OP, drop 2 xs)
      else (TK_AMP, xt)
   | x == '^' =
      if "=" `isPrefixOf` xt then (TK_XOR_ASSIGN, drop 2 xs)
      else (TK_XOR, xt)
   | x == '|' =
      if "=" `isPrefixOf` xt then (TK_OR_ASSIGN, drop 2 xs)
      else if "|" `isPrefixOf` xt then (TK_OR_OP, drop 2 xs)
      else (TK_PIPE, xt)
   | x == '=' =
      if "=" `isPrefixOf` xt then (TK_EQ_OP, drop 2 xs)
      else (TK_ASSIGN, xt)
   | x == '!' =
      if "=" `isPrefixOf` xt then (TK_NE_OP, drop 2 xs)
      else (TK_NOT, xt)
   | x == ';' = (TK_SEMICOLON, xt)
   | x == '{' = (TK_LEFT_CURLY_BRACKET, xt)
   | x == '}' = (TK_RIGHT_CURLY_BRACKET, xt)
   | x == ',' = (TK_COMMA, xt)
   | x == ':' =
      if ">" `isPrefixOf` xt then (TK_RIGHT_SQUARE_BRACKET, drop 2 xs)
      else (TK_COLON, xt)
   | x == '(' = (TK_LEFT_PAREN, xt)
   | x == ')' = (TK_RIGHT_PAREN, xt)
   | x == '[' = (TK_LEFT_SQUARE_BRACKET, xt)
   | x == ']' = (TK_RIGHT_SQUARE_BRACKET, xt)
   | x == '~' = (TK_TILDE, xt)
   | x == '?' = (TK_QUESTION_MARK, xt)
   | otherwise = error ("Unexpected token " ++ show x)

consumeBlockComment :: String -> String
consumeBlockComment "" = error "Block comment not closed"
consumeBlockComment xs@(_:xt)
   | "*/" `isPrefixOf` xs = tail xt
   | otherwise            = consumeBlockComment xt

consumeCppStyleComment :: String -> String
consumeCppStyleComment "" = ""
consumeCppStyleComment (x:xs)
   | x == '\n' = xs
   | otherwise = consumeCppStyleComment xs

getIdentifier :: String -> (Token, String)
getIdentifier "" = (TK_EOF, "")
getIdentifier xs =
   case iden of
      "auto" ->     (TK_AUTO, rest)
      "break" ->    (TK_BREAK, rest)
      "case" ->     (TK_CASE, rest)
      "char" ->     (TK_CHAR, rest)
      "const" ->    (TK_CONST, rest)
      "continue" -> (TK_CONTINUE, rest)
      "default" ->  (TK_DEFAULT, rest)
      "do" ->       (TK_DO, rest)
      "double" ->   (TK_DOUBLE, rest)
      "else" ->     (TK_ELSE, rest)
      "enum" ->     (TK_ENUM, rest)
      "extern" ->   (TK_EXTERN, rest)
      "float" ->    (TK_FLOAT, rest)
      "for" ->      (TK_FOR, rest)
      "goto" ->     (TK_GOTO, rest)
      "if" ->       (TK_IF, rest)
      "inline" ->   (TK_INLINE, rest)
      "int" ->      (TK_INT, rest)
      "long" ->     (TK_LONG, rest)
      "register" -> (TK_REGISTER, rest)
      "restrict" -> (TK_RESTRICT, rest)
      "return" ->   (TK_RETURN, rest)
      "short" ->    (TK_SHORT, rest)
      "signed" ->   (TK_SIGNED, rest)
      "sizeof" ->   (TK_SIZEOF, rest)
      "static" ->   (TK_STATIC, rest)
      "struct" ->   (TK_STRUCT, rest)
      "switch" ->   (TK_SWITCH, rest)
      "typedef" ->  (TK_TYPEDEF, rest)
      "union" ->    (TK_UNION, rest)
      "unsigned" -> (TK_UNSIGNED, rest)
      "void" ->     (TK_VOID, rest)
      "volatile" -> (TK_VOLATILE, rest)
      "while" ->    (TK_WHILE, rest)
      _ ->          (TK_IDENTIFIER iden, rest)
   where iden = takeWhile isLD xs
         rest = drop (length iden) xs

-- A constant is either an integer literal or floating point literal
getConstant :: String -> (Token, String)
getConstant "" = (TK_EOF, "")
getConstant xs = (TK_CONSTANT con, rest)
   where con = takeWhile (\x -> isDigit x || x == '.') xs
         rest = drop (length con) xs

getStringLiteral :: String -> (Token, String)
getStringLiteral xs = (TK_STRING_LITERAL str, rest)
   where str = takeWhile (not . (== '"')) xs
         rest = drop (length str + 1) xs -- +1 for closing "
