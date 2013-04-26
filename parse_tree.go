package parser

// (function-definition | declaration)+
type translationUnitNode struct {
	nodes []struct {
		fdef *functionDefinitionNode
		decl *declarationNode
	}
}

// declaration-specifiers? declarator declaration* block
type functionDefinitionNode struct {
	dspecs *declarationSpecifiersNode
	decl   declaratorNode
	decls  *[]declarationNode
	blk    blockNode
}

// declaration-specifiers init-declarator% ";"
type declarationNode struct {
	decls  declarationSpecifiersNode
	idecls []initDeclaratorNode
}

// (storage-class-specifier | type-specifier | type-qualifier)+
type declarationSpecifiersNode struct {
	nodes []struct {
		scs *storageClassSpecifierNode
		ts  *typeSpecifierNode
		tq  *typeQualifierNode
	}
}

// "auto" | "register" | "static" | "extern" | "typedef"
type storageClassSpecifierNode TokenType

// "void" | "char" | "short" | "int" | "long" | "float" | "double" | "signed" |
// "unsigned" | struct-or-union-specifier | enum-specifier | typedef-name
type typeSpecifierNode struct {
	nodes []struct {
		tk  *TokenType
		sus *structOrUnionSpecifierNode
		es  *enumSpecifierNode
		tn  *typedefName
	}
}

// "const" | "volatile"
type typeQualifierNode TokenType

// ("struct" | "union") (
//		identifier? "{" struct-declaration+ "}" |
//		identifier
// )
type structOrUnionSpecifierNode struct {
	tk    TokenType
	nodes struct {
		id1   *Token
		sdecl *[]structDeclarationNode

		id2 *Token
	}
}

// declarator ("=" initializer)?
type initDeclaratorNode struct {
	decl declaratorNode
	i    *initializerNode
}

// (type-specifier | type-qualifier)+ struct-declarator%
type structDeclarationNode struct {
	mods []struct {
		ts *typeSpecifierNode
		tq *typeQualifierNode
	}

	sdecls []structDeclarationNode
}

// declarator | declarator? ":" constant-expression
type structDeclaratorNode struct {
	decl *declaratorNode
	cex  *constantExpressionNode
}

// "enum" (identifier | identifier? "{" enumerator% "}")
type enumSpecifierNode struct {
	id    *Token
	enums *[]enumeratorNode
}

// identifier ("=" constant-expression)?
type enumeratorNode struct {
	id  Token
	cex *constantExpressionNode
}

// pointer? (identifier | "(" declarator ")") (
//		"[" constant-expression? "]" |
//		"(" parameter-type-list ")" |
//		"(" identifier%? ")"
// )*
type declaratorNode struct {
	p *pointerNode

	id   *Token
	decl *declaratorNode

	nodes *[]struct {
		cex *constantExpressionNode
		ptl *parameterTypeListNode
		ids *[]Token
	}
}

// ("*" type-qualifier*)*
type pointerNode struct {
	tqs *[]*typeQualifierNode // Yes, *[]*
}

// parameter-declaration% ("," "...")?
type parameterTypeListNode struct {
	pdecls    []parameterDeclarationNode
	isVarArgs bool
}

// declaration-specifiers (declarator | abstract-declarator)?
type parameterDeclarationNode struct {
	decls declarationSpecifiersNode

	decl  *declaratorNode
	adecl *abstractDeclaratorNode
}

// assignment-expression | "{" initializer% ","? "}"
type initializerNode struct {
	aex  *assignmentExpressionNode
	inis *[]initializerNode
}

// (type-specifier | type-qualifier)+ abstract-declarator?
type typeNameNode struct {
	mods []struct {
		ts *typeSpecifierNode
		tq *typeQualifierNode
	}

	adecl *abstractDeclaratorNode
}

// pointer ("(" abstract-declarator ")")? (
//		"[" constant-expression? "]" |
//		"(" parameter-type-list? ")"
// )*
type abstractDeclaratorNode struct {
	p     pointerNode
	adecl *abstractDeclaratorNode

	nodes *[]struct {
		cex *constantExpressionNode
		ptl *parameterTypeListNode
	}
}

// ((identifier | "case" constant-expression | "default") ":")*
// (expression? ";" |
//  block |
//  "if" "(" expression ")" statement |
//  "if" "(" expression ")" statement "else" statement |
//  "switch" "(" expression ")" statement |
//  "while" "(" expression ")" statement |
//  "do" statement "while" "(" expression ")" ";" |
//  "for" "(" expression? ";" expression? ";" expression?
//  ")" statement |
//  "goto" identifier ";" |
//  "continue" ";" |
//  "break" ";" |
//  "return" expression? ";"
// )
type statementNode struct {
	nodes *[]struct {
		id  *Token
		cex *constantExpressionNode
		d   *TokenType // TK_DEFAULT
	}

	expr  *expressionNode
	block *blockNode
	i     *ifNode // if, if-else
	s     *switchNode
	w     *whileNode
	dw    *doWhileNode
	f     *forNode
	g     *gotoNode
	c     *continueNode
	b     *breakNode
	r     *returnNode
}

// "if" "(" expression ")" statement ("else" statement)?
type ifNode struct {
	e  expressionNode
	s1 statementNode
	s2 *statementNode
}

// "switch" "(" expression ")" statement
type switchNode struct {
	e expressionNode
	s statementNode
}

// "while" "(" expression ")" statement
type whileNode struct {
	e expressionNode
	s statementNode
}

// "do" statement "while" "(" expression ")" ";"
type doWhileNode struct {
	e expressionNode
	s statementNode
}

// "for" "(" expression? ";" expression? ";" expression? ")" statement
type forNode struct {
	e1 *expressionNode
	e2 *expressionNode
	e3 *expressionNode
	s  statementNode
}

// "goto" identifier ";"
type gotoNode struct {
	id Token
}

// "continue" ";"
type continueNode struct{}

// "break" ";"
type breakNode struct{}

// "return" expression? ";"
type returnNode struct {
	e *expressionNode
}

// "{" declaration* statement* "}"
type blockNode struct {
	decls *[]declarationNode
	ss    *[]statementNode
}

// assignment-expression%
type expressionNode struct {
	aexs []assignmentExpressionNode
}

// (
//	unary-expression (
//	 "=" | "*=" | "/=" | "%=" | "+=" | "-=" | "<<=" | ">>=" | "&=" | "^=" | "|="
//  )
// )* conditional-expression
type assignmentExpressionNode struct {
	exprs *[]struct {
		uex unaryExpressionNode
		tk  TokenType
	}

	cex conditionalExpressionNode
}

// logical-OR-expression ( "?" expression ":" conditional-expression )?
type conditionalExpressionNode struct {
	loex logicalOrExpressionNode
	e    *expressionNode
	cex  *conditionalExpressionNode
}

// conditional-expression
type constantExpressionNode struct {
	cex conditionalExpressionNode
}

// logical-AND-expression ( "||" logical-AND-expression )*
type logicalOrExpressionNode struct {
	laexs []logicalAndExpressionNode
}

//   inclusive-OR-expression ( "&&" inclusive-OR-expression )*
type logicalAndExpressionNode struct {
	ioexs []inclusiveOrExpressionNode
}

//   exclusive-OR-expression ( "|" exclusive-OR-expression )*
type inclusiveOrExpressionNode struct {
	eoexs []exclusiveOrExpressionNode
}

//   AND-expression ( "^" AND-expression )*
type exclusiveOrExpressionNode struct {
	aexs []andExpressionNode
}

// equality-expression ( "&" equality-expression )*
type andExpressionNode struct {
	eqexs []equalityExpressionNode
}

// relational-expression ( ("==" | "!=") relational-expression )*
type equalityExpressionNode struct {
	rex  relationalExpressionNode
	rexs []struct {
		tk  TokenType
		rex relationalExpressionNode
	}
}

// shift-expression ( ("<" | ">" | "<=" | ">=") shift-expression )*
type relationalExpressionNode struct {
	sex  shiftExpressionNode
	sexs []struct {
		tk  TokenType
		sex shiftExpressionNode
	}
}

// additive-expression ( ("<<" | ">>") additive-expression )*
type shiftExpressionNode struct {
	aex  additiveExpressionNode
	aexs []struct {
		tk  TokenType
		aex additiveExpressionNode
	}
}

// multiplicative-expression ( ("+" | "-") multiplicative-expression )*
type additiveExpressionNode struct {
	mex  multiplicativeExpressionNode
	mexs []struct {
		tk  TokenType
		mex multiplicativeExpressionNode
	}
}

// cast-expression ( ("*" | "/" | "%" ) cast-expression )*
type multiplicativeExpressionNode struct {
	cex  castExpressionNode
	cexs []struct {
		tk  TokenType
		cex castExpressionNode
	}
}

// ( "(" type-name ")" )* unary-expression
type castExpressionNode struct {
	tns *[]typeNameNode
	uex unaryExpressionNode
}

// ("++" | "--" | "sizeof" )* (
//		"sizeof" "(" type-name ")"                           |
//		("&" | "*" | "+" | "-" | "~" | "!" ) cast-expression |
//		postfix-expression
// )
type unaryExpressionNode struct {
	tks *[]TokenType

	tn *typeNameNode // sizeof this

	tk  *TokenType // for cast expression
	cex *castExpressionNode

	pex *postfixExpressionNode
}

// (identifier | constant | string | "(" expression ")") (
//		"[" expression "]"             |
//		"(" assignment-expression% ")" |
//		"." identifier                 |
//		"->" identifier                |
//		"++"                           |
//		"--"
// )*
type postfixExpressionNode struct {
	id  *Token
	c   *Token
	str *Token
	e   *expressionNode

	data *[]struct {
		e    *expressoinNode
		aexs *[]assignmentExpressionNode

		tk *TokenType // . -> ++ --
		id *Token
	}
}
