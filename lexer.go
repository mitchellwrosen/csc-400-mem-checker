package main

import (
	"fmt"
	"strings"
	"unicode"
)

const (
	EOF = -1

	DIGITS     = "0123456789"
	HEX_DIGITS = "abcdefABCDEF0123456789"
)

type Lexer struct {
	input string // the string being scanned

	start int // start position of this token
	pos   int // current position in the input

	tokens []Token
}

// Lexes the given C code.
//
// param input	the C code to lex
//
// returns	a slice of tokens
func Lex(input string) []Token {
	l := &Lexer{
		input:  input,
		tokens: make([]Token, 0, 100),
	}

	l.run()
	return l.tokens
}

// Gets the next rune in the input and advances the position.
//
// returns	the next rune
func (l *Lexer) next() rune {
	if l.pos >= len(l.input) {
		return EOF
	}

	r := l.input[l.pos]
	l.pos++
	return rune(r)
}

// Gets the next rune in the input without advancing the position.
//
// returns	The next rune
func (l *Lexer) peek() rune {
	if l.pos >= len(l.input) {
		return EOF
	}
	return rune(l.input[l.pos])
}

func (l *Lexer) peekBy(n int) string {
	runes := make([]rune, n)
	for i := 0; i < n; i++ {
		runes[i] = l.next()
	}
	l.backupBy(n)

	return string(runes)
}

// Gets the current value of the lexer
func (l *Lexer) val() string {
	return l.input[l.start:l.pos]
}

func (l *Lexer) ignore() {
	l.start = l.pos
}

func (l *Lexer) advance() {
	l.pos++
}

func (l *Lexer) advanceBy(n int) {
	l.pos += n
}

func (l *Lexer) backup() {
	l.pos--
}

func (l *Lexer) backupBy(n int) {
	l.pos -= n
}

// possibly consumes a character in |valid|
func (l *Lexer) accept(valid string) bool {
	r := l.next()
	if strings.ContainsRune(valid, r) {
		return true
	}
	if r != EOF {
		l.backup()
	}
	return false
}

// consumes characters until one not in |valid| is found
func (l *Lexer) acceptRun(valid string) int {
	var run int
	var r rune
	for r = l.next(); strings.ContainsRune(valid, r); r = l.next() {
		run++
	}
	if r != EOF {
		l.backup()
	}
	return run
}

// consume whitespace
func (l *Lexer) acceptWhitespaceRun() {
	var r rune
	for r = l.next(); unicode.IsSpace(r); r = l.next() {
	}
	if r != EOF {
		l.backup()
	}
}

// consume characters until whitespace {
func (l *Lexer) acceptNonWhitespaceRun() {
	var r rune
	for r = l.next(); !unicode.IsSpace(r); r = l.next() {
	}
	if r != EOF {
		l.backup()
	}
}

// peeks, returns whether or not the char is in |valid|
func (l *Lexer) peekAccept(valid string) bool {
	if strings.ContainsRune(valid, l.peek()) {
		return true
	}
	return false
}

// peeks until char not in |valid|, returns number in |valid|
func (l *Lexer) peekAcceptRun(valid string) int {
	var run int
	var r rune
	for r = l.next(); strings.ContainsRune(valid, r); r = l.next() {
		run++
	}

	// We consumed at all valid plus the first invalid - first valid may have been
	// EOF, though, in which case pos wasn't incremented.
	l.backupBy(run)
	if r != EOF {
		l.backup()
	}

	return run
}

// Sends an error token to the channel, then returns nil to end the state
// function loop in run()
func (l *Lexer) errorf(format string, args ...interface{}) action {
	l.tokens = append(l.tokens, Token{
		tkError,
		fmt.Sprintf(format, args...),
	})

	return nil
}

// Appends a token with value [start, pos), then advances start to pos.
//
// param	t	the type of the token to append.
func (l *Lexer) appendToken(t TokenType) {
	l.tokens = append(l.tokens, Token{t, l.val()})
	l.start = l.pos
}

func (l *Lexer) run() {
	for state := lexCode; state != nil; {
		state = state(l)
	}
}

// action represents the state of the lexer as a function that returns the
// next state
type action func(l *Lexer) action

func lexCode(l *Lexer) action {
	if l.isEOF() {
		return lexEOF(l)
	} else if l.isWhitespace() {
		return lexWhitespace(l)
	} else if l.isBlockComment() {
		return lexBlockComment(l)
	} else if l.isCppStyleComment() {
		return lexCppStyleComment(l)
	} else if l.isIdentifier() {
		return lexIdentifier(l)
	} else if l.isConstant() {
		return lexConstant(l)
	} else if l.isStringLiteral() {
		return lexStringLiteral(l)
	} else if l.isOther() {
		return lexOther(l)
	}
	panic("NOTREACHED")
}

func (l *Lexer) isEOF() bool {
	return l.peek() == EOF
}

func lexEOF(l *Lexer) action {
	l.appendToken(tkEOF)
	return nil
}

func (l *Lexer) isWhitespace() bool {
	return unicode.IsSpace(l.peek())
}

func lexWhitespace(l *Lexer) action {
	l.acceptWhitespaceRun()
	l.ignore()
	return lexCode
}

// "/*"
func (l *Lexer) isBlockComment() bool {
	defer l.backup() // matched by single call to next()

	if c := l.next(); c == '/' {
		if c2 := l.peek(); c2 == '*' {
			return true
		} else {
			return false
		}
	}
	return false
}

func lexBlockComment(l *Lexer) action {
	l.advanceBy(2) // consume "/*"
	for c := l.next(); c != EOF; c = l.next() {
		if c == '*' {
			var c2 rune
			for c2 = l.next(); c2 == '*'; c2 = l.next() {
			}

			if c2 == '/' {
				l.ignore()
				return lexCode
			}
		}
	}

	return l.errorf("unterminated comment")
}

func (l *Lexer) isCppStyleComment() bool {
	defer l.backup() // matched by single call to next()

	if c := l.next(); c == '/' {
		if c2 := l.peek(); c2 == '/' {
			return true
		} else {
			return false
		}
	}
	return false
}

func lexCppStyleComment(l *Lexer) action {
	l.advanceBy(2) // consume "//"

	for c := l.next(); c != '\n' && c != EOF; c = l.next() {
	}

	l.ignore()
	return lexCode
}

// keyword, or {L}({L}|{D})* where
// {L} = [a-zA-Z_]
// {D} = [0-9]
func (l *Lexer) isIdentifier() bool {
	defer l.backup() // matched by single call to next()

	if c := l.next(); isLetter(c) || c == '_' {
		// Could be constant or string literal
		if c == 'L' {
			if c2 := l.peek(); c2 == '\'' || c2 == '"' {
				return false
			}
		}
		return true
	}
	return false
}

func lexIdentifier(l *Lexer) action {
	l.acceptRun("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_")

	switch id := l.val(); id {
	case "auto":
		l.appendToken(tkAuto)
	case "break":
		l.appendToken(tkBreak)
	case "case":
		l.appendToken(tkCase)
	case "char":
		l.appendToken(tkChar)
	case "const":
		l.appendToken(tkConst)
	case "continue":
		l.appendToken(tkContinue)
	case "default":
		l.appendToken(tkDefault)
	case "do":
		l.appendToken(tkDo)
	case "double":
		l.appendToken(tkDouble)
	case "else":
		l.appendToken(tkElse)
	case "enum":
		l.appendToken(tkEnum)
	case "extern":
		l.appendToken(tkExtern)
	case "float":
		l.appendToken(tkFloat)
	case "for":
		l.appendToken(tkFor)
	case "goto":
		l.appendToken(tkGoto)
	case "if":
		l.appendToken(tkIf)
	case "inline":
		l.appendToken(tkInline)
	case "int":
		l.appendToken(tkInt)
	case "long":
		l.appendToken(tkLong)
	case "register":
		l.appendToken(tkRegister)
	case "restrict":
		l.appendToken(tkRestrict)
	case "return":
		l.appendToken(tkReturn)
	case "short":
		l.appendToken(tkShort)
	case "signed":
		l.appendToken(tkSigned)
	case "sizeof":
		l.appendToken(tkSizeof)
	case "static":
		l.appendToken(tkStatic)
	case "struct":
		l.appendToken(tkStruct)
	case "switch":
		l.appendToken(tkSwitch)
	case "typedef":
		l.appendToken(tkTypedef)
	case "union":
		l.appendToken(tkUnion)
	case "unsigned":
		l.appendToken(tkUnsigned)
	case "void":
		l.appendToken(tkVoid)
	case "volatile":
		l.appendToken(tkVolatile)
	case "while":
		l.appendToken(tkWhile)
	default:
		l.appendToken(tkIdentifier)
	}

	return lexCode
}

// See www.quut.com/c/ANSI-C-grammar-l-2011.html
// Doesn't catch {CP}?"'"([^'\\\n]|{ES})+"'" yet
func (l *Lexer) isConstant() bool {
	defer l.backup() // matched by single call to next()

	r := l.next()
	if strings.ContainsRune(DIGITS, r) ||
		r == '.' && strings.ContainsRune(DIGITS, l.peek()) {
		return true
	}

	return false
}

// Lexes a number constant. Has very relaxed rules about what is actually legal
// C, because gcc can deal with all that shit. All I care about is something
// that looks number-ey, which includes, for example, the following known bugs:
//		- Octals outside of the octal range, ex. "08"
//		- No digits after [eE] or [pP], ex. "5.00e"
//		- Multiple suffixes of the same type, ex. "100lLuUfF"
func lexConstant(l *Lexer) action {
	digits := DIGITS
	if l.accept("0") && l.accept("xX") {
		digits = HEX_DIGITS
	}

	l.acceptRun(digits)
	if l.accept(".") {
		l.acceptRun(digits)
	}

	if l.accept("eE") {
		l.accept("+-")
		l.acceptRun(DIGITS)
	}

	if l.accept("pP") {
		l.accept("+-")
		l.acceptRun(DIGITS)
	}

	l.acceptRun("fFlLuU")

	// constant fully parsed - look for invalid suffix (an alphanum)
	if next := l.peek(); isAlphaNum(next) {
		l.ignore()                 // discard the entire constant
		l.acceptNonWhitespaceRun() // get the invalid suffix to report
		return l.errorf("invalid suffix \"%s\" on constant", l.val())
	}

	l.appendToken(tkConstant)
	return lexCode
}

func (l *Lexer) isStringLiteral() bool {
	// TODO
	return false
}

func lexStringLiteral(l *Lexer) action {
	// TODO
	return lexCode
}

// catch-all for symbols, operators, etc.
func (l *Lexer) isOther() bool {
	return true
}

func lexOther(l *Lexer) action {
	switch c := l.next(); c {
	case '.':
		if l.peekBy(2) == ".." {
			l.advanceBy(2)
			l.appendToken(tkEllipsis)
		} else {
			l.appendToken(tkDot)
		}
	case '>':
		if l.peekBy(2) == ">=" {
			l.advanceBy(2)
			l.appendToken(tkRightAssign)
		} else if l.peek() == '>' {
			l.advance()
			l.appendToken(tkRightOp)
		} else if l.peek() == '=' {
			l.advance()
			l.appendToken(tkGeOp)
		} else {
			l.appendToken(tkGtOp)
		}
	case '<':
		if l.peekBy(2) == "<=" {
			l.advanceBy(2)
			l.appendToken(tkLeftAssign)
		} else if l.peek() == '<' {
			l.advance()
			l.appendToken(tkLeftOp)
		} else if l.peek() == '=' {
			l.advance()
			l.appendToken(tkLeOp)
		} else if l.peek() == '%' {
			l.advance()
			l.appendToken(tkLeftCurlyBracket)
		} else if l.peek() == ':' {
			l.advance()
			l.appendToken(tkLeftSquareBracket)
		} else {
			l.appendToken(tkLtOp)
		}
	case '+':
		if l.peek() == '=' {
			l.advance()
			l.appendToken(tkAddAssign)
		} else if l.peek() == '+' {
			l.advance()
			l.appendToken(tkIncOp)
		} else {
			l.appendToken(tkPlus)
		}
	case '-':
		if l.peek() == '=' {
			l.advance()
			l.appendToken(tkSubAssign)
		} else if l.peek() == '-' {
			l.advance()
			l.appendToken(tkDecOp)
		} else if l.peek() == '>' {
			l.advance()
			l.appendToken(tkPtrOp)
		} else {
			l.appendToken(tkMinus)
		}
	case '*':
		if l.peek() == '=' {
			l.advance()
			l.appendToken(tkMulAssign)
		} else {
			l.appendToken(tkStar)
		}
	case '/':
		if l.peek() == '=' {
			l.advance()
			l.appendToken(tkDivAssign)
		} else {
			l.appendToken(tkDiv)
		}
	case '%':
		if l.peek() == '=' {
			l.advance()
			l.appendToken(tkModAssign)
		} else if l.peek() == '>' {
			l.advance()
			l.appendToken(tkRightCurlyBracket)
		} else {
			l.appendToken(tkMod)
		}
	case '&':
		if l.peek() == '=' {
			l.advance()
			l.appendToken(tkAndAssign)
		} else if l.peek() == '&' {
			l.advance()
			l.appendToken(tkAndOp)
		} else {
			l.appendToken(tkAmpersand)
		}
	case '^':
		if l.peek() == '=' {
			l.advance()
			l.appendToken(tkXorAssign)
		} else {
			l.appendToken(tkCarrot)
		}
	case '|':
		if l.peek() == '=' {
			l.advance()
			l.appendToken(tkOrAssign)
		} else if l.peek() == '|' {
			l.advance()
			l.appendToken(tkOrOp)
		} else {
			l.appendToken(tkPipe)
		}
	case '=':
		if l.peek() == '=' {
			l.advance()
			l.appendToken(tkEqOp)
		} else {
			l.appendToken(tkAssign)
		}
	case '!':
		if l.peek() == '=' {
			l.advance()
			l.appendToken(tkNeOp)
		} else {
			l.appendToken(tkBang)
		}
	case ';':
		l.appendToken(tkSemicolon)
	case '{':
		l.appendToken(tkLeftCurlyBracket)
	case '}':
		l.appendToken(tkLeftCurlyBracket)
	case ',':
		l.appendToken(tkComma)
	case ':':
		if l.peek() == '>' {
			l.advance()
			l.appendToken(tkRightSquareBracket)
		} else {
			l.appendToken(tkColon)
		}
	case '(':
		l.appendToken(tkLeftParen)
	case ')':
		l.appendToken(tkRightParen)
	case '[':
		l.appendToken(tkLeftSquareBracket)
	case ']':
		l.appendToken(tkRightSquareBracket)
	case '~':
		l.appendToken(tkTilde)
	case '?':
		l.appendToken(tkQuestionMark)
	default:
		l.ignore()
	}

	return lexCode
}
