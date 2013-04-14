package main

type symbol int

type Parser struct {
	tokens []Token

	pos int // index into s

	tk Token
}

func Parse(input string) {
	p := &Parser{
		tokens: Lex(input),
	}
	p.next()
	parseTranslationUnit(p)
}

// Gets the next token, and returns whether or not it's EOF.
func (p *Parser) next() bool {
	p.tk = p.tokens[p.pos]
	if p.tk.typ == tkEOF {
		return true
	}
	p.pos++
	return false
}

func (p *Parser) peek() Token {
	return p.tokens[p.pos]
}

func (p *Parser) atEOF() bool {
	return p.peek().typ == tkEOF
}

// Checks to see if the current token matches |typ|, and advances the current
// token regardless.
func (p *Parser) matches(typ TokenType) bool {
	t := p.tkType()
	p.next()
	return t == typ
}

func (p *Parser) tkType() TokenType {
	return p.tk.typ
}

////////////////////////////////////////////////////////////////////////////////

type ParseMethod func(*Parser) bool

// Wraps matches(), but returns a ParseMethod (to more cleanly pass to allOf()
func matches_(p *Parser, typ TokenType) ParseMethod {
	return func(*Parser) bool {
		return p.matches(typ)
	}
}

// Runs the |m| one or more times with receiver |p|. The parser's position will
// be just after the last successful parse. Returns whether or not there was at
// least one non-terminal to parse.
func oneOrMore(p *Parser, m ParseMethod) ParseMethod {
	return func(*Parser) bool {
		if !m(p) {
			return false
		}

		zeroOrMore(p, m)(p)
		return true
	}
}

// Runs the |m| zero or more times with receiver |p|. The parser's position will
// be just after the last successful parse (possibly zero).
func zeroOrMore(p *Parser, m ParseMethod) ParseMethod {
	return func(*Parser) bool {
		for optional(p, m)(p) {
		}
		return true
	}
}

// Matches a non-null comma-separated list of symbols.
func percent(p *Parser, m ParseMethod) ParseMethod {
	return func(*Parser) bool {
		if !m(p) {
			return false
		}

		return zeroOrMore(p, func(*Parser) bool {
			return p.matches(tkComma) && m(p)
		})(p)
	}
}

// Possibly parses using |m| on |p|. Returns whether or not anything was parsed.
func optional(p *Parser, m ParseMethod) ParseMethod {
	return func(*Parser) bool {
		save := p.pos
		if !m(p) {
			p.pos = save
			return false
		}
		return true
	}
}

// Tries each of |ms| in order, returns whether or not any of them were
// successful.
func anyOf(p *Parser, ms ...ParseMethod) ParseMethod {
	return func(*Parser) bool {
		save := p.pos

		for _, m := range ms {
			p.pos = save
			if m(p) {
				return true
			}
		}

		return false
	}
}

func allOf(p *Parser, ms ...ParseMethod) ParseMethod {
	return func(*Parser) bool {
		for _, m := range ms {
			if !m(p) {
				return false
			}
		}
		return true
	}
}

////////////////////////////////////////////////////////////////////////////////

// (function-definition | declaration)+
func parseTranslationUnit(p *Parser) bool {
	return oneOrMore(p,
		anyOf(p,
			parseFunctionDefinition,
			parseDeclaration,
		),
	)(p)
}

// declaration-specifiers? declarator declaration* block
func parseFunctionDefinition(p *Parser) bool {
	return allOf(p,
		optional(p, parseDeclarationSpecifiers),
		parseDeclarator,
		zeroOrMore(p, parseDeclaration),
		parseBlock,
	)(p)
}

// declaration-specifiers init-declarator% ";"
func parseDeclaration(p *Parser) bool {
	return allOf(p,
		parseDeclarationSpecifiers,
		percent(p, parseInitDeclarator),
		matches_(p, tkSemicolon),
	)(p)
}

// (storage-class-specifier | type-specifier | type-qualifier)+
func parseDeclarationSpecifiers(p *Parser) bool {
	return oneOrMore(p,
		anyOf(p,
			parseStorageClassSpecifier,
			parseTypeSpecifier,
			parseTypeQualifier,
		),
	)(p)
}

func parseStorageClassSpecifier(p *Parser) bool {
	return anyOf(p,
		matches_(p, tkAuto),
		matches_(p, tkRegister),
		matches_(p, tkStatic),
		matches_(p, tkExtern),
		matches_(p, tkTypedef),
	)(p)
}

func parseTypeSpecifier(p *Parser) bool {
	return anyOf(p,
		matches_(p, tkVoid),
		matches_(p, tkChar),
		matches_(p, tkShort),
		matches_(p, tkInt),
		matches_(p, tkLong),
		matches_(p, tkFloat),
		matches_(p, tkDouble),
		matches_(p, tkSigned),
		matches_(p, tkUnsigned),
		parseStructOrUnionSpecifier,
		parseEnumSpecifier,
		matches_(p, tkTypedefName),
	)(p)
}

func parseTypeQualifier(p *Parser) bool {
	return anyOf(p,
		matches_(p, tkConst),
		matches_(p, tkVolatile),
	)(p)
}

// ("struct" | "union") (
//		identifier? "{" struct-declaration+ "}" |
//		identifier
func parseStructOrUnionSpecifier(p *Parser) bool {
	return allOf(p,
		anyOf(p,
			matches_(p, tkStruct),
			matches_(p, tkUnion),
		),
		anyOf(p,
			allOf(p,
				optional(p,
					matches_(p, tkIdentifier),
				),
				matches_(p, tkLeftCurlyBracket),
				oneOrMore(p, parseStructDeclaration),
				matches_(p, tkRightCurlyBracket),
			),
			matches_(p, tkIdentifier),
		),
	)(p)

}

func parseInitDeclarator(p *Parser) bool {
	return allOf(p,
		parseDeclarator,
		optional(p,
			allOf(p,
				matches_(p, tkAssign),
				parseInitializer,
			),
		),
	)(p)
}

// (type-specifier | type-qualifier)+ struct-declarator%
func parseStructDeclaration(p *Parser) bool {
	return allOf(p,
		oneOrMore(p,
			anyOf(p,
				parseTypeSpecifier,
				parseTypeQualifier,
			),
		),
		percent(p, parseStructDeclarator),
	)(p)
}

// declarator | declarator? ":" constant-expression
func parseStructDeclarator(p *Parser) bool {
	return anyOf(p,
		parseDeclarator,
		allOf(p,
			optional(p, parseDeclarator),
			matches_(p, tkColon),
			parseConstantExpression,
		),
	)(p)
}

// "enum" (identifier | identifier? "{" enumerator% "}")
func parseEnumSpecifier(p *Parser) bool {
	return allOf(p,
		matches_(p, tkEnum),
		anyOf(p,
			matches_(p, tkIdentifier),
			allOf(p,
				optional(p,
					matches_(p, tkIdentifier),
				),
				matches_(p, tkLeftCurlyBracket),
				percent(p, parseEnumerator),
				matches_(p, tkRightCurlyBracket),
			),
		),
	)(p)
}

// identifier ("=" constant-expression)?
func parseEnumerator(p *Parser) bool {
	return allOf(p,
		matches_(p, tkIdentifier),
		optional(p,
			allOf(p,
				matches_(p, tkAssign),
				parseConstantExpression,
			),
		),
	)(p)
}

// pointer? (identifier | "(" declarator ")") (
//		"[" constant-expression? "]" |
//		"(" parameter-type-list ")" |
//		"(" identifier%? ")"
// )*
func parseDeclarator(p *Parser) bool {
	return allOf(p,
		optional(p, parsePointer),
		anyOf(p,
			matches_(p, tkIdentifier),
			allOf(p,
				matches_(p, tkLeftParen),
				parseDeclarator,
				matches_(p, tkRightParen),
			),
		),
		zeroOrMore(p,
			anyOf(p,
				allOf(p,
					matches_(p, tkLeftSquareBracket),
					optional(p, parseConstantExpression),
					matches_(p, tkRightSquareBracket),
				),
				allOf(p,
					matches_(p, tkLeftParen),
					parseParameterTypeList,
					matches_(p, tkRightParen),
				),
				allOf(p,
					matches_(p, tkLeftParen),
					optional(p,
						percent(p,
							matches_(p, tkIdentifier),
						),
					),
					matches_(p, tkRightParen),
				),
			),
		),
	)(p)
}

// ("*" type-qualifier*)*
func parsePointer(p *Parser) bool {
	return zeroOrMore(p,
		allOf(p,
			matches_(p, tkStar),
			zeroOrMore(p, parseTypeQualifier),
		),
	)(p)
}

// parameter-declaration% ("," "...")?
func parseParameterTypeList(p *Parser) bool {
	return allOf(p,
		percent(p, parseParameterDeclaration),
		optional(p,
			allOf(p,
				matches_(p, tkComma),
				matches_(p, tkEllipsis),
			),
		),
	)(p)
}

// declaration-specifiers (declarator | abstract-declarator)?
func parseParameterDeclaration(p *Parser) bool {
	return allOf(p,
		parseDeclarationSpecifiers,
		optional(p,
			anyOf(p,
				parseDeclarator,
				parseAbstractDeclarator,
			),
		),
	)(p)
}

// assignment-expression | "{" initializer% ","? "}"
func parseInitializer(p *Parser) bool {
	return anyOf(p,
		parseAssignmentExpression,
		allOf(p,
			matches_(p, tkLeftCurlyBracket),
			percent(p, parseInitializer),
			optional(p,
				matches_(p, tkComma),
			),
			matches_(p, tkRightCurlyBracket),
		),
	)(p)
}

// (type-specifier | type-qualifier)+ abstract-declarator?
func parseTypeName(p *Parser) bool {
	return allOf(p,
		oneOrMore(p,
			anyOf(p,
				parseTypeSpecifier,
				parseTypeQualifier,
			),
		),
		optional(p, parseAbstractDeclarator),
	)(p)
}

// pointer ("(" abstract-declarator ")")? (
//		"[" constant-expression? "]" |
//		"(" parameter-type-list? ")"
// )*
func parseAbstractDeclarator(p *Parser) bool {
	return allOf(p,
		parsePointer,
		optional(p,
			allOf(p,
				matches_(p, tkLeftParen),
				parseAbstractDeclarator,
				matches_(p, tkRightParen),
			),
		),
		zeroOrMore(p,
			anyOf(p,
				allOf(p,
					matches_(p, tkLeftSquareBracket),
					optional(p, parseConstantExpression),
					matches_(p, tkRightSquareBracket),
				),
				allOf(p,
					matches_(p, tkLeftParen),
					optional(p, parseParameterTypeList),
					matches_(p, tkRightParen),
				),
			),
		),
	)(p)
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
func parseStatement(p *Parser) bool {
	return allOf(p,
		zeroOrMore(p,
			allOf(p,
				anyOf(p,
					matches_(p, tkIdentifier),
					allOf(p,
						matches_(p, tkCase),
						parseConstantExpression,
					),
					matches_(p, tkDefault),
				),
				matches_(p, tkColon),
			),
		),
		anyOf(p,
			allOf(p,
				optional(p, parseExpression),
				matches_(p, tkColon),
			),
			parseBlock,
			allOf(p,
				matches_(p, tkIf),
				matches_(p, tkLeftParen),
				parseExpression,
				matches_(p, tkRightParen),
				parseStatement,
			),
			allOf(p,
				matches_(p, tkIf),
				matches_(p, tkLeftParen),
				parseExpression,
				matches_(p, tkRightParen),
				parseStatement,
				matches_(p, tkElse),
				parseStatement,
			),
			allOf(p,
				matches_(p, tkSwitch),
				matches_(p, tkLeftParen),
				parseExpression,
				matches_(p, tkRightParen),
				parseStatement,
			),
			allOf(p,
				matches_(p, tkWhile),
				matches_(p, tkLeftParen),
				parseExpression,
				matches_(p, tkRightParen),
				parseStatement,
			),
			allOf(p,
				matches_(p, tkDo),
				parseStatement,
				matches_(p, tkWhile),
				matches_(p, tkLeftParen),
				parseExpression,
				matches_(p, tkRightParen),
				matches_(p, tkSemicolon),
			),
			allOf(p,
				matches_(p, tkFor),
				matches_(p, tkLeftParen),
				optional(p, parseExpression),
				matches_(p, tkSemicolon),
				optional(p, parseExpression),
				matches_(p, tkSemicolon),
				optional(p, parseExpression),
				matches_(p, tkRightParen),
				parseStatement,
			),
			allOf(p,
				matches_(p, tkGoto),
				matches_(p, tkIdentifier),
				matches_(p, tkSemicolon),
			),
			allOf(p,
				matches_(p, tkContinue),
				matches_(p, tkSemicolon),
			),
			allOf(p,
				matches_(p, tkBreak),
				matches_(p, tkSemicolon),
			),
			allOf(p,
				matches_(p, tkReturn),
				optional(p, parseExpression),
				matches_(p, tkSemicolon),
			),
		),
	)(p)
}

// "{" declaration* statement* "}"
func parseBlock(p *Parser) bool {
	return allOf(p,
		matches_(p, tkLeftCurlyBracket),
		zeroOrMore(p, parseDeclaration),
		zeroOrMore(p, parseStatement),
		matches_(p, tkRightCurlyBracket),
	)(p)
}

// assignment-expression%
func parseExpression(p *Parser) bool {
	return percent(p, parseAssignmentExpression)(p)
}

// (
//	unary-expression (
//	 "=" | "*=" | "/=" | "%=" | "+=" | "-=" | "<<=" | ">>=" | "&=" | "^=" | "|="
//  )
// )* conditional-expression
func parseAssignmentExpression(p *Parser) bool {
	return allOf(p,
		zeroOrMore(p,
			allOf(p,
				parseUnaryExpression,
				anyOf(p,
					matches_(p, tkAssign),
					matches_(p, tkMulAssign),
					matches_(p, tkDivAssign),
					matches_(p, tkModAssign),
					matches_(p, tkAddAssign),
					matches_(p, tkSubAssign),
					matches_(p, tkLeftAssign),
					matches_(p, tkRightAssign),
					matches_(p, tkAndAssign),
					matches_(p, tkXorAssign),
					matches_(p, tkOrAssign),
				),
			),
		),
		parseConditionalExpression,
	)(p)
}

// logical-OR-expression ( "?" expression ":" conditional-expression )?
func parseConditionalExpression(p *Parser) bool {
	return allOf(p,
		parseLogicalOrExpression,
		optional(p,
			allOf(p,
				matches_(p, tkQuestionMark),
				parseExpression,
				matches_(p, tkColon),
				parseConditionalExpression,
			),
		),
	)(p)
}

// conditional-expression
func parseConstantExpression(p *Parser) bool {
	return parseConditionalExpression(p)
}

//   logical-AND-expression ( "||" logical-AND-expression )*
func parseLogicalOrExpression(p *Parser) bool {
	return allOf(p,
		parseLogicalAndExpression,
		zeroOrMore(p,
			allOf(p,
				matches_(p, tkOrOp),
				parseLogicalAndExpression,
			),
		),
	)(p)
}

//   inclusive-OR-expression ( "&&" inclusive-OR-expression )*
func parseLogicalAndExpression(p *Parser) bool {
	return allOf(p,
		parseInclusiveOrExpression,
		zeroOrMore(p,
			allOf(p,
				matches_(p, tkAndOp),
				parseInclusiveOrExpression,
			),
		),
	)(p)
}

//   exclusive-OR-expression ( "|" exclusive-OR-expression )*
func parseInclusiveOrExpression(p *Parser) bool {
	return allOf(p,
		parseExclusiveOrExpression,
		zeroOrMore(p,
			allOf(p,
				matches_(p, tkPipe),
				parseExclusiveOrExpression,
			),
		),
	)(p)
}

//   AND-expression ( "^" AND-expression )*
func parseExclusiveOrExpression(p *Parser) bool {
	return allOf(p,
		parseAndExpression,
		zeroOrMore(p,
			allOf(p,
				matches_(p, tkCarrot),
				parseAndExpression,
			),
		),
	)(p)
}

// equality-expression ( "&" equality-expression )*
func parseAndExpression(p *Parser) bool {
	return allOf(p,
		parseEqualityExpression,
		zeroOrMore(p,
			allOf(p,
				matches_(p, tkAmpersand),
				parseEqualityExpression,
			),
		),
	)(p)
}

// relational-expression ( ("==" | "!=") relational-expression )*
func parseEqualityExpression(p *Parser) bool {
	return allOf(p,
		parseRelationalExpression,
		zeroOrMore(p,
			allOf(p,
				anyOf(p,
					matches_(p, tkEqOp),
					matches_(p, tkNeOp),
				),
				parseRelationalExpression,
			),
		),
	)(p)
}

// shift-expression ( ("<" | ">" | "<=" | ">=") shift-expression )*
func parseRelationalExpression(p *Parser) bool {
	return allOf(p,
		parseShiftExpression,
		zeroOrMore(p,
			allOf(p,
				anyOf(p,
					matches_(p, tkLtOp),
					matches_(p, tkGtOp),
					matches_(p, tkLeOp),
					matches_(p, tkGeOp),
				),
				parseShiftExpression,
			),
		),
	)(p)
}

// additive-expression ( ("<<" | ">>") additive-expression )*
func parseShiftExpression(p *Parser) bool {
	return allOf(p,
		parseAdditiveExpression,
		zeroOrMore(p,
			allOf(p,
				anyOf(p,
					matches_(p, tkLeftOp),
					matches_(p, tkRightOp),
				),
				parseAdditiveExpression,
			),
		),
	)(p)
}

// multiplicative-expression ( ("+" | "-") multiplicative-expression )*
func parseAdditiveExpression(p *Parser) bool {
	return allOf(p,
		parseMultiplicativeExpression,
		zeroOrMore(p,
			allOf(p,
				anyOf(p,
					matches_(p, tkPlus),
					matches_(p, tkMinus),
				),
				parseMultiplicativeExpression,
			),
		),
	)(p)
}

// cast-expression ( ("*" | "/" | "%" ) cast-expression )*
func parseMultiplicativeExpression(p *Parser) bool {
	return allOf(p,
		parseCastExpression,
		zeroOrMore(p,
			allOf(p,
				anyOf(p,
					matches_(p, tkStar),
					matches_(p, tkDiv),
					matches_(p, tkMod),
				),
				parseCastExpression,
			),
		),
	)(p)
}

// ( "(" type-name ")" )* unary-expression
func parseCastExpression(p *Parser) bool {
	return allOf(p,
		zeroOrMore(p,
			allOf(p,
				matches_(p, tkLeftParen),
				parseTypeName,
				matches_(p, tkRightParen),
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
func parseUnaryExpression(p *Parser) bool {
	return allOf(p,
		zeroOrMore(p,
			allOf(p,
				matches_(p, tkIncOp),
				matches_(p, tkDecOp),
				matches_(p, tkSizeof),
			),
		),
		anyOf(p,
			allOf(p,
				matches_(p, tkSizeof),
				matches_(p, tkLeftParen),
				parseTypeName,
				matches_(p, tkRightParen),
			),
			allOf(p,
				anyOf(p,
					matches_(p, tkAmpersand),
					matches_(p, tkStar),
					matches_(p, tkPlus),
					matches_(p, tkMinus),
					matches_(p, tkTilde),
					matches_(p, tkBang),
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
func parsePostfixExpression(p *Parser) bool {
	allOf(p,
		anyOf(p,
			matches_(p, tkIdentifier),
			parseConstant,
			matches_(p, tkStringLiteral),
			allOf(p,
				matches_(p, tkLeftParen),
				parseExpression,
				matches_(p, tkRightParen),
			),
		),
		zeroOrMore(p,
			anyOf(p,
				allOf(p,
					matches_(p, tkLeftSquareBracket),
					parseExpression,
					matches_(p, tkRightSquareBracket),
				),
				allOf(p,
					matches_(p, tkLeftParen),
					percent(p, parseAssignmentExpression),
					matches_(p, tkRightParen),
				),
				allOf(p,
					matches_(p, tkDot),
					matches_(p, tkIdentifier),
				),
				allOf(p,
					matches_(p, tkPtrOp),
					matches_(p, tkIdentifier),
				),
				matches_(p, tkIncOp),
				matches_(p, tkDecOp),
			),
		),
	)(p)
}

// integer-constant | character-constant | floating-constant | enumeration-constant
func parseConstant(p *Parser) bool {
	return matches_(p, tkConstant)(p) // Only one type of constant, for now
}
