package parser

////////////////////////////////////////////////////////////////////////////////

// (function-definition | declaration)+
type translationUnitNode struct {
	data []struct {
		data []interface{}
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
	decls  []declarationSpecifiersNode
	idecls []initDeclaratorNode
}

// (storage-class-specifier | type-specifier | type-qualifier)+
type declarationSpecifiersNode struct {
	data []struct {
		data []interface{}
	}
}

// "auto" | "register" | "static" | "extern" | "typedef"
type storageClassSpecifierNode TokenType

// "void" | "char" | "short" | "int" | "long" | "float" | "double" | "signed" |
// "unsigned" | struct-or-union-specifier | enum-specifier | typedef-name
type typeSpecifierNode struct {
	data []interface{}
}

// "const" | "volatile"
type typeQualifierNode TokenType

// ("struct" | "union") (
//		identifier? "{" struct-declaration+ "}" |
//		identifier
type structOrUnionSpecifierNode struct {
	tk   TokenType
	data interface{}
}

//		identifier? "{" struct-declaration+ "}" |
type structOrUnionSpecifierNode_ struct {
	id     *structIdentifierNode
	sdecls []structDeclarationNode
}

// declarator ("=" initializer)?
type initDeclaratorNode struct {
	decl declaratorNode
	ins  *[]initializerNode
}

// (type-specifier | type-qualifier)+ struct-declarator%
type structDeclarationNode struct {
	mods   []interface{}
	sdecls []structDeclarationNode
}

// declarator | declarator? ":" constant-expression
type structDeclaratorNode struct {
	decl *declaratorNode
	cex  *constantExpression
}

// "enum" (identifier | identifier? "{" enumerator% "}")
type enumSpecifierNode struct {
	id    *identifierNode
	enums *[]enumeratorNode
}

// identifier ("=" constant-expression)?
type enumeratorNode struct {
	id  identifierNode
	cex *constantExpression
}

// pointer? (identifier | "(" declarator ")") (
//		"[" constant-expression? "]" |
//		"(" parameter-type-list ")" |
//		"(" identifier%? ")"
// )*
type declaratorNode struct {
	p *pointerNode

	id   *identifierNode
	decl *declaratorNode

	cexs *[]constantExpressionNode
	ptl  *parameterTypeListNode
	ids  *[]identifierNode
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
	mods  []interface{}
	adecl *abstractDeclaratorNode
}

// pointer ("(" abstract-declarator ")")? (
//		"[" constant-expression? "]" |
//		"(" parameter-type-list? ")"
// )*
type abstractDeclaratorNode struct {
	p    pointerNode
	data *[]interface{}
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
	data *[]interface{}

	expr  *expressionNode
	block *blockNode
	i     *ifNode // covers if, if-else
	s     *switchNode
	w     *whileNode
	dw    *doWhileNode
	f     *forNode
	g     *gotoNode
	c     *continueNode
	b     *breakNode
	r     *returnNode
}

type ifNode struct {
	e  expressionNode
	s1 statementNode
	s2 *statementNode
}

type switchNode struct {
	e expressionNode
	s statementNode
}

type whileNode struct {
	e expressionNode
	s statementNode
}

type doWhileNode struct {
	e expressionNode
	s statementNode
}

type forNode struct {
	e1 *expressionNode
	e2 *expressionNode
	e3 *expressionNode
	s  statementNode
}

type gotoNode struct {
	id identifierNode
}

type continueNode struct{}

type breakNode struct{}

type returnNode struct {
	e *expressionNode
}

/* TODO: the rest, and refactor above

func parseStatement(p *Parser) error {
	return allOf(
		zeroOrMore(
			allOf(
				anyOf(
					matches(tkIdentifier),
					allOf(
						matches(tkCase),
						parseConstantExpression,
					),
					matches(tkDefault),
				),
				matches(tkColon),
			),
		),
		anyOf(
			allOf(
				optional(parseExpression),
				matches(tkColon),
			),
			parseBlock,
			allOf(
				matches(tkIf),
				matches(tkLeftParen),
				parseExpression,
				matches(tkRightParen),
				parseStatement,
			),
			allOf(
				matches(tkIf),
				matches(tkLeftParen),
				parseExpression,
				matches(tkRightParen),
				parseStatement,
				matches(tkElse),
				parseStatement,
			),
			allOf(
				matches(tkSwitch),
				matches(tkLeftParen),
				parseExpression,
				matches(tkRightParen),
				parseStatement,
			),
			allOf(
				matches(tkWhile),
				matches(tkLeftParen),
				parseExpression,
				matches(tkRightParen),
				parseStatement,
			),
			allOf(
				matches(tkDo),
				parseStatement,
				matches(tkWhile),
				matches(tkLeftParen),
				parseExpression,
				matches(tkRightParen),
				matches(tkSemicolon),
			),
			allOf(
				matches(tkFor),
				matches(tkLeftParen),
				optional(parseExpression),
				matches(tkSemicolon),
				optional(parseExpression),
				matches(tkSemicolon),
				optional(parseExpression),
				matches(tkRightParen),
				parseStatement,
			),
			allOf(
				matches(tkGoto),
				matches(tkIdentifier),
				matches(tkSemicolon),
			),
			allOf(
				matches(tkContinue),
				matches(tkSemicolon),
			),
			allOf(
				matches(tkBreak),
				matches(tkSemicolon),
			),
			allOf(
				matches(tkReturn),
				optional(parseExpression),
				matches(tkSemicolon),
			),
		),
	)(p)
}

// "{" declaration* statement* "}"
func parseBlock(p *Parser) error {
	return allOf(
		matches(tkLeftCurlyBracket),
		zeroOrMore(parseDeclaration),
		zeroOrMore(parseStatement),
		matches(tkRightCurlyBracket),
	)(p)
}

// assignment-expression%
func parseExpression(p *Parser) error {
	return percent(parseAssignmentExpression)(p)
}

// (
//	unary-expression (
//	 "=" | "*=" | "/=" | "%=" | "+=" | "-=" | "<<=" | ">>=" | "&=" | "^=" | "|="
//  )
// )* conditional-expression
func parseAssignmentExpression(p *Parser) error {
	return allOf(
		zeroOrMore(
			allOf(
				parseUnaryExpression,
				anyOf(
					matches(tkAssign),
					matches(tkMulAssign),
					matches(tkDivAssign),
					matches(tkModAssign),
					matches(tkAddAssign),
					matches(tkSubAssign),
					matches(tkLeftAssign),
					matches(tkRightAssign),
					matches(tkAndAssign),
					matches(tkXorAssign),
					matches(tkOrAssign),
				),
			),
		),
		parseConditionalExpression,
	)(p)
}

// logical-OR-expression ( "?" expression ":" conditional-expression )?
func parseConditionalExpression(p *Parser) error {
	return allOf(
		parseLogicalOrExpression,
		optional(
			allOf(
				matches(tkQuestionMark),
				parseExpression,
				matches(tkColon),
				parseConditionalExpression,
			),
		),
	)(p)
}

// conditional-expression
func parseConstantExpression(p *Parser) error {
	return parseConditionalExpression(p)
}

//   logical-AND-expression ( "||" logical-AND-expression )*
func parseLogicalOrExpression(p *Parser) error {
	return allOf(
		parseLogicalAndExpression,
		zeroOrMore(
			allOf(
				matches(tkOrOp),
				parseLogicalAndExpression,
			),
		),
	)(p)
}

//   inclusive-OR-expression ( "&&" inclusive-OR-expression )*
func parseLogicalAndExpression(p *Parser) error {
	return allOf(
		parseInclusiveOrExpression,
		zeroOrMore(
			allOf(
				matches(tkAndOp),
				parseInclusiveOrExpression,
			),
		),
	)(p)
}

//   exclusive-OR-expression ( "|" exclusive-OR-expression )*
func parseInclusiveOrExpression(p *Parser) error {
	return allOf(
		parseExclusiveOrExpression,
		zeroOrMore(
			allOf(
				matches(tkPipe),
				parseExclusiveOrExpression,
			),
		),
	)(p)
}

//   AND-expression ( "^" AND-expression )*
func parseExclusiveOrExpression(p *Parser) error {
	return allOf(
		parseAndExpression,
		zeroOrMore(
			allOf(
				matches(tkCarrot),
				parseAndExpression,
			),
		),
	)(p)
}

// equality-expression ( "&" equality-expression )*
func parseAndExpression(p *Parser) error {
	return allOf(
		parseEqualityExpression,
		zeroOrMore(
			allOf(
				matches(tkAmpersand),
				parseEqualityExpression,
			),
		),
	)(p)
}

// relational-expression ( ("==" | "!=") relational-expression )*
func parseEqualityExpression(p *Parser) error {
	return allOf(
		parseRelationalExpression,
		zeroOrMore(
			allOf(
				anyOf(
					matches(tkEqOp),
					matches(tkNeOp),
				),
				parseRelationalExpression,
			),
		),
	)(p)
}

// shift-expression ( ("<" | ">" | "<=" | ">=") shift-expression )*
func parseRelationalExpression(p *Parser) error {
	return allOf(
		parseShiftExpression,
		zeroOrMore(
			allOf(
				anyOf(
					matches(tkLtOp),
					matches(tkGtOp),
					matches(tkLeOp),
					matches(tkGeOp),
				),
				parseShiftExpression,
			),
		),
	)(p)
}

// additive-expression ( ("<<" | ">>") additive-expression )*
func parseShiftExpression(p *Parser) error {
	return allOf(
		parseAdditiveExpression,
		zeroOrMore(
			allOf(
				anyOf(
					matches(tkLeftOp),
					matches(tkRightOp),
				),
				parseAdditiveExpression,
			),
		),
	)(p)
}

// multiplicative-expression ( ("+" | "-") multiplicative-expression )*
func parseAdditiveExpression(p *Parser) error {
	return allOf(
		parseMultiplicativeExpression,
		zeroOrMore(
			allOf(
				anyOf(
					matches(tkPlus),
					matches(tkMinus),
				),
				parseMultiplicativeExpression,
			),
		),
	)(p)
}

// cast-expression ( ("*" | "/" | "%" ) cast-expression )*
func parseMultiplicativeExpression(p *Parser) error {
	return allOf(
		parseCastExpression,
		zeroOrMore(
			allOf(
				anyOf(
					matches(tkStar),
					matches(tkDiv),
					matches(tkMod),
				),
				parseCastExpression,
			),
		),
	)(p)
}

// ( "(" type-name ")" )* unary-expression
func parseCastExpression(p *Parser) error {
	return allOf(
		zeroOrMore(
			allOf(
				matches(tkLeftParen),
				parseTypeName,
				matches(tkRightParen),
			),
		),
		parseUnaryExpression,
	)(p)
}

// ("++" | "--" | "sizeof" )* (
//		"sizeof" "(" type-name ")"                           |
//		("&" | "*" | "+" | "-" | "~" | "!" ) cast-expression |
//		postfix-expression
// )
func parseUnaryExpression(p *Parser) error {
	return allOf(
		zeroOrMore(
			allOf(
				matches(tkIncOp),
				matches(tkDecOp),
				matches(tkSizeof),
			),
		),
		anyOf(
			allOf(
				matches(tkSizeof),
				matches(tkLeftParen),
				parseTypeName,
				matches(tkRightParen),
			),
			allOf(
				anyOf(
					matches(tkAmpersand),
					matches(tkStar),
					matches(tkPlus),
					matches(tkMinus),
					matches(tkTilde),
					matches(tkBang),
				),
				parseCastExpression,
			),
			parsePostfixExpression,
		),
	)(p)
}

// (identifier | constant | string | "(" expression ")") (
//		"[" expression "]"             |
//		"(" assignment-expression% ")" |
//		"." identifier                 |
//		"->" identifier                |
//		"++"                           |
//		"--"
// )*
func parsePostfixExpression(p *Parser) error {
	return allOf(
		anyOf(
			matches(tkIdentifier),
			parseConstant,
			matches(tkStringLiteral),
			allOf(
				matches(tkLeftParen),
				parseExpression,
				matches(tkRightParen),
			),
		),
		zeroOrMore(
			anyOf(
				allOf(
					matches(tkLeftSquareBracket),
					parseExpression,
					matches(tkRightSquareBracket),
				),
				allOf(
					matches(tkLeftParen),
					percent(parseAssignmentExpression),
					matches(tkRightParen),
				),
				allOf(
					matches(tkDot),
					matches(tkIdentifier),
				),
				allOf(
					matches(tkPtrOp),
					matches(tkIdentifier),
				),
				matches(tkIncOp),
				matches(tkDecOp),
			),
		),
	)(p)
}

// integer-constant | character-constant | floating-constant | enumeration-constant
func parseConstant(p *Parser) error {
	return matches(tkConstant)(p) // Only one type of constant, for now
}

*/
