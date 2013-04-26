package main

import (
	"fmt"
)

type symbol int

type Parser struct {
	tokens []Token
	pos    int   // index into tokens
	tk     Token // current token
}

func Parse(input string) error {
	p := &Parser{
		tokens: Lex(input),
	}
	return parseTranslationUnit(p)
}

// Advances the parser's current token
func (p *Parser) next() {
	p.tk = p.tokens[p.pos]
	if p.tk.typ != tkEOF {
		p.pos++
	}
}

////////////////////////////////////////////////////////////////////////////////

type ParseMethod func(p *Parser) (interface{}, error)

// Checks to see if the current token matches |typ|, and advances the current
// token regardless.
func matches(typ TokenType) ParseMethod {
	return func(p *Parser) (interface{}, error) {
		p.next()
		if p.tk.typ != typ {
			return nil, fmt.Errorf("Expected %v (got %v)", typ, p.tk.typ)
		}
		return typ, nil
	}
}

// Possibly parses using |m| on |p|. Returns no error.
func optional(m ParseMethod) ParseMethod {
	return func(p *Parser) (interface{}, error) {
		save := p.pos
		node, err := m(p)
		if err != nil {
			p.pos = save
		}
		return node, nil
	}
}

// Runs the |m| zero or more times with receiver |p|. The parser's position will
// be just after the last successful parse (possibly zero). Returns no error.
func zeroOrMore(m ParseMethod) ParseMethod {
	return func(p *Parser) (interface{}, error) {
		nodes := make([]interface{}, 0, 1)
		save := p.pos
		for ; ; save = p.pos {
			node, err := m(p)
			if err != nil {
				break
			}
			nodes = append(nodes, node)
		}
		p.pos = save

		// Made nodes already, backpedal if nothing was added to it
		if len(nodes) == 0 {
			return nil, nil
		}
		return nodes, nil
	}
}

// Runs the |m| one or more times with receiver |p|. The parser's position will
// be just after the last successful parse. Returns whether or not there was at
// least one non-terminal to parse.
func oneOrMore(m ParseMethod) ParseMethod {
	return func(p *Parser) (interface{}, error) {
		nodes := make([]interface{}, 0, 1)

		node, err := m(p)
		if err != nil {
			return nil, err
		}
		nodes = append(nodes, node)

		nodes2, err := zeroOrMore(m)(p)
		if err != nil {
			return nil, err
		}

		if nodes2 != nil {
			nodes = append(nodes, nodes2.([]interface{})...)
		}

		return nodes, nil
	}
}

// Matches a non-null comma-separated list of symbols.
func percent(m ParseMethod) ParseMethod {
	return func(p *Parser) (interface{}, error) {
		nodes := make([]interface{}, 0, 1)

		node, err := m(p)
		if err != nil {
			return nil, err
		}

		nodes2, err := zeroOrMore(
			allOf(
				matches(tkComma),
				m,
			),
		)(p)

		if err != nil {
			return nil, err
		}

		if nodes2 != nil {
			// nodes2 is []interface{}, where each elem is a []interface{} of
			// length 2, with [0] == tkComma, [1] == node. Extract the nodes.
			nodes3 := make([]interface{}, 0, len(nodes2.([]interface{})))
			for _, n := range nodes2.([]interface{}) {
				nodes3 = append(nodes3, n.([]interface{})[1])
			}
			nodes = append(nodes, nodes3...)
		}

		return nodes, nil
	}
}

// Tries each of |ms| in order, returns whether or not any of them were
// successful.
func anyOf(ms ...ParseMethod) ParseMethod {
	return func(p *Parser) (interface{}, error) {
		var err error
		save := p.pos

		for _, m := range ms {
			p.pos = save

			node, err := m(p)
			if err == nil {
				return node, nil
			}
		}

		return nil, fmt.Errorf("Could not match any of %v", ms) // TODO fix
	}
}

func allOf(ms ...ParseMethod) ParseMethod {
	return func(p *Parser) (interface{}, error) {
		nodes := make([]interface{}, 0, len(ms))

		for _, m := range ms {
			node, err := m(p)
			if err != nil {
				return nil, err
			}
			nodes = append(nodes, node)
		}
		return nodes, nil
	}
}

////////////////////////////////////////////////////////////////////////////////

// (function-definition | declaration)+
func parseTranslationUnit(p *Parser) error {
	return oneOrMore(
		anyOf(
			parseFunctionDefinition,
			parseDeclaration,
		),
	)(p)
}

// declaration-specifiers? declarator declaration* block
func parseFunctionDefinition(p *Parser) (Node, error) {
	return allOf(
		optional(parseDeclarationSpecifiers),
		parseDeclarator,
		zeroOrMore(parseDeclaration),
		parseBlock,
	)(p)
}

// declaration-specifiers init-declarator% ";"
func parseDeclaration(p *Parser) error {
	return allOf(
		parseDeclarationSpecifiers,
		percent(parseInitDeclarator),
		matches(tkSemicolon),
	)(p)
}

// (storage-class-specifier | type-specifier | type-qualifier)+
func parseDeclarationSpecifiers(p *Parser) error {
	return oneOrMore(
		anyOf(
			parseStorageClassSpecifier,
			parseTypeSpecifier,
			parseTypeQualifier,
		),
	)(p)
}

// ("auto" | "register" | "static" | "extern" | "typedef")
func parseStorageClassSpecifier(p *Parser) error {
	return anyOf(
		matches(tkAuto),
		matches(tkRegister),
		matches(tkStatic),
		matches(tkExtern),
		matches(tkTypedef),
	)(p)
}

// "void" | "char" | "short" | "int" | "long" | "float" | "double" | "signed" |
// "unsigned" | struct-or-union-specifier | enum-specifier | typedef-name
func parseTypeSpecifier(p *Parser) error {
	return anyOf(
		matches(tkVoid),
		matches(tkChar),
		matches(tkShort),
		matches(tkInt),
		matches(tkLong),
		matches(tkFloat),
		matches(tkDouble),
		matches(tkSigned),
		matches(tkUnsigned),
		parseStructOrUnionSpecifier,
		parseEnumSpecifier,
		//matches(tkTypedefName), // TODO: figure out what this is
	)(p)
}

// "const" | "volatile"
func parseTypeQualifier(p *Parser) error {
	return anyOf(
		matches(tkConst),
		matches(tkVolatile),
	)(p)
}

// ("struct" | "union") (
//		identifier? "{" struct-declaration+ "}" |
//		identifier
func parseStructOrUnionSpecifier(p *Parser) error {
	return allOf(
		anyOf(
			matches(tkStruct),
			matches(tkUnion),
		),
		anyOf(
			allOf(
				optional(matches(tkIdentifier)),
				matches(tkLeftCurlyBracket),
				oneOrMore(parseStructDeclaration),
				matches(tkRightCurlyBracket),
			),
			matches(tkIdentifier),
		),
	)(p)
}

// declarator ("=" initializer)?
func parseInitDeclarator(p *Parser) error {
	return allOf(
		parseDeclarator,
		optional(
			allOf(
				matches(tkAssign),
				parseInitializer,
			),
		),
	)(p)
}

// (type-specifier | type-qualifier)+ struct-declarator%
func parseStructDeclaration(p *Parser) error {
	return allOf(
		oneOrMore(
			anyOf(
				parseTypeSpecifier,
				parseTypeQualifier,
			),
		),
		percent(parseStructDeclarator),
	)(p)
}

// declarator | declarator? ":" constant-expression
func parseStructDeclarator(p *Parser) error {
	return anyOf(
		parseDeclarator,
		allOf(
			optional(parseDeclarator),
			matches(tkColon),
			parseConstantExpression,
		),
	)(p)
}

// "enum" (identifier | identifier? "{" enumerator% "}")
func parseEnumSpecifier(p *Parser) error {
	return allOf(
		matches(tkEnum),
		anyOf(
			matches(tkIdentifier),
			allOf(
				optional(matches(tkIdentifier)),
				matches(tkLeftCurlyBracket),
				percent(parseEnumerator),
				matches(tkRightCurlyBracket),
			),
		),
	)(p)
}

// identifier ("=" constant-expression)?
func parseEnumerator(p *Parser) error {
	return allOf(
		matches(tkIdentifier),
		optional(
			allOf(
				matches(tkAssign),
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
func parseDeclarator(p *Parser) error {
	return allOf(
		optional(parsePointer),
		anyOf(
			matches(tkIdentifier),
			allOf(
				matches(tkLeftParen),
				parseDeclarator,
				matches(tkRightParen),
			),
		),
		zeroOrMore(
			anyOf(
				allOf(
					matches(tkLeftSquareBracket),
					optional(parseConstantExpression),
					matches(tkRightSquareBracket),
				),
				allOf(
					matches(tkLeftParen),
					parseParameterTypeList,
					matches(tkRightParen),
				),
				allOf(
					matches(tkLeftParen),
					optional(percent(matches(tkIdentifier))),
					matches(tkRightParen),
				),
			),
		),
	)(p)
}

// ("*" type-qualifier*)*
func parsePointer(p *Parser) error {
	return zeroOrMore(
		allOf(
			matches(tkStar),
			zeroOrMore(parseTypeQualifier),
		),
	)(p)
}

// parameter-declaration% ("," "...")?
func parseParameterTypeList(p *Parser) error {
	return allOf(
		percent(parseParameterDeclaration),
		optional(
			allOf(
				matches(tkComma),
				matches(tkEllipsis),
			),
		),
	)(p)
}

// declaration-specifiers (declarator | abstract-declarator)?
func parseParameterDeclaration(p *Parser) error {
	return allOf(
		parseDeclarationSpecifiers,
		optional(
			anyOf(
				parseDeclarator,
				parseAbstractDeclarator,
			),
		),
	)(p)
}

// assignment-expression | "{" initializer% ","? "}"
func parseInitializer(p *Parser) error {
	return anyOf(
		parseAssignmentExpression,
		allOf(
			matches(tkLeftCurlyBracket),
			percent(parseInitializer),
			optional(matches(tkComma)),
			matches(tkRightCurlyBracket),
		),
	)(p)
}

// (type-specifier | type-qualifier)+ abstract-declarator?
func parseTypeName(p *Parser) error {
	return allOf(
		oneOrMore(
			anyOf(
				parseTypeSpecifier,
				parseTypeQualifier,
			),
		),
		optional(parseAbstractDeclarator),
	)(p)
}

// pointer ("(" abstract-declarator ")")? (
//		"[" constant-expression? "]" |
//		"(" parameter-type-list? ")"
// )*
func parseAbstractDeclarator(p *Parser) error {
	return allOf(
		parsePointer,
		optional(
			allOf(
				matches(tkLeftParen),
				parseAbstractDeclarator,
				matches(tkRightParen),
			),
		),
		zeroOrMore(
			anyOf(
				allOf(
					matches(tkLeftSquareBracket),
					optional(parseConstantExpression),
					matches(tkRightSquareBracket),
				),
				allOf(
					matches(tkLeftParen),
					optional(parseParameterTypeList),
					matches(tkRightParen),
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
