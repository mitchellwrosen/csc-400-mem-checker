module Parser where

{-
data AST = Empty
         | Node ASTNode
-}

{-
data ASTNode = ASTOperator Operator
          | ASTExpression Expression
          | ASTStatement Statement
-}

{-
data Operator = OP_RIGHT -- >>
              | OP_LEFT  -- <<
              | OP_INC   -- ++
              | OP_DEC   -- --
              | OP_PTR   -- ->
              | OP_AND   -- &&
              | OP_OR    -- ||
              | OP_DOT   -- .
              | OP_AMP   -- &
              | OP_XOR   -- ^
              | OP_PIPE  -- |
              | OP_NOT   -- !
              | OP_TILDE -- ~
              | OP_MINUS -- -
              | OP_PLUS  -- +
              | OP_TIMES -- *
              | OP_DIV   -- /
              | OP_MOD   -- %
              | OP_LT    -- <
              | OP_GT    -- >
              | OP_LE    -- <=
              | OP_GE    -- >=
              | OP_EQ    -- ==
              | OP_NE    -- !=
              | OP_QM    -- ?
              | OP_SIZEOF -- "sizeof"
              | AOP
              | AOP_MUL
              | AOP_DIV
              | AOP_MOD
              | AOP_PLUS
              | AOP_MINUS
              | AOP_LEFT
              | AOP_RIGHT
              | AOP_AND
              | AOP_XOR
              | AOP_OR
-}

data Operator =
   -- Unary
   OP_Index   -- []
 | OP_Dot     -- .
 | OP_Arrow   -- ->
 | OP_PostInc -- ++
 | OP_PostDec -- --
 | OP_PreInc  -- ++
 | OP_PreDec  -- --
 | OP_AddrOf  -- &
 | OP_Deref   -- *
 | OP_Pos     -- +
 | OP_Neg     -- -
 | OP_Tilde   -- ~
 | OP_Not     -- !
 | OP_Sizeof  -- sizeof
 -- Binary
 | OP_Mult       -- *
 | OP_Div        -- /
 | OP_Mod        -- %
 | OP_Plus       -- +
 | OP_Minus      -- -
 | OP_LeftShift  -- <<
 | OP_RightShift -- >>
 | OP_Lt         -- <
 | OP_Gt         -- >
 | OP_Lte        -- <=
 | OP_Gte        -- >=
 | OP_Eq         -- ==
 | OP_Neq        -- !=
 | OP_BitAnd     -- &
 | OP_BitXor     -- ^
 | OP_BitOr      -- |
 | OP_And        -- &&
 | OP_Or         -- ||
 -- Assignment
 | OP_Assign      -- =
 | OP_MultAssign  -- *=
 | OP_DivAssign   -- /=
 | OP_ModAssign   -- %=
 | OP_PlusAssign  -- +=
 | OP_MinusAssign -- -=
 | OP_LeftAssign  -- <<=
 | OP_RightAssign -- >>=
 | OP_AndAssign   -- &=
 | OP_XorAssign   -- ^=
 | OP_OrAssign    -- |=

type Identifier = String
type Typename = String
type Constant = String
data Expression = EXP_Identifier Identifier
                | EXP_Constant Constant
                | EXP_StringLiteral String
                | EXP_Unary Operator Expression
                | EXP_Binary Operator Expression Expression
                | EXP_SizeofTypename Typename Expression
                | EXP_CondOp Expression Expression Expression

data Declaration = DECL [DeclarationSpecifier] [InitDeclarator]

data DeclarationSpecifier = DS_StorageClassSpec StorageClassSpecifier
                          | DS_TypeSpec TypeSpecifier
                          | DS_TypeQual TypeQualifier

data InitDeclarator = ID_Uninit Declarator
                    | ID_Init Declarator Initializer

data StorageClassSpecifier = SCS_Typedef
                           | SCS_Extern
                           | SCS_Static
                           | SCS_Auto
                           | SCS_Register

data TypeSpecifier = TS_Void
                   | TS_Char
                   | TS_Short
                   | TS_Int
                   | TS_Long
                   | TS_Float
                   | TS_Double
                   | TS_Signed
                   | TS_Unsigned
                   | TS_StructOrUnionSpec StructOrUnionSpecifier
                   | TS_EnumSpec EnumSpecifier
                   | TS_TypedefName Identifier

data StructOrUnionSpecifier = SUS_Init StructOrUnion (Maybe Identifier) [StructDeclaration]
                            | SUS_Uninit StructOrUnion Identifier

data StructOrUnion = Struct | Union

type StructDeclaration = ([SpecifierQualifier], [StructDeclarator])

data SpecifierQualifier = SQ_Specs [TypeSpecifier]
                        | SQ_Quals [TypeQualifier]

data StructDeclarator = SD_Decl Declarator
                      | SD_Expr (Maybe Declarator) Expression

data EnumSpecifier = ES_List (Maybe Identifier) [Enumerator]
                   | ES_Enum Identifier

data Enumerator = E_Uninit EnumerationConstant
                | E_Init EnumerationConstant Expression

data TypeQualifier = TQ_Const | TQ_Volatile

type Declarator = ((Maybe Pointer), DirectDeclarator)

data DirectDeclarator = DD_Ident Identifier
                      | DD_Decl Declarator
                      | DD_Size (Maybe Expression)
                      | DD_Params ParameterTypeList
                      | DD_Idents [Identifier]

type Pointer = [[TypeQualifier]]

data ParameterTypeList = PTL [Parameter] Bool -- isVarArgs



--- TODO


data Initializer = INIT_Expr Expression
                 | INIT_List [Initializer]


data Statement = ST_Label Identifier Statement
               | ST_Case Expression Statement
               | ST_Default Statement
               | ST_Compound [Declaration] [Statement]
               | ST_Expr (Maybe Expression)
               | ST_If Expression Statement (Maybe Statement)
               | ST_Switch Expression Statement
               | ST_While Expression Statement
               | ST_DoWhile Statement Expression
               | ST_For (Maybe Expression) (Maybe Expression) (Maybe Expression) Statement
               | ST_Goto Identifier
               | ST_Continue
               | ST_Break
               | ST_Return (Maybe Expression)

{-cparse :: [Token] -> AST-}
