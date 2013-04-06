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

type lexer struct {
	input  string     // the string being scanned
	start  int        // start position of this token
	pos    int        // current position in the input
	tokens chan token // channel of scanned token
}

// Lexes the given C code.
//
// param input	The C code to lex
//
// returns	A pointer to the new lexer
//			A channel of tokens to receive from
func lex(input string) (*lexer, chan token) {
	l := &lexer{
		input:  input,
		tokens: make(chan token, 10),
	}
	go l.run()
	return l, l.tokens
}

// Gets the next rune in the input and advances the position.
//
// returns	The next rune
func (l *lexer) next() rune {
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
func (l *lexer) peek() rune {
	if l.pos >= len(l.input) {
		return EOF
	}
	return rune(l.input[l.pos])
}

func (l *lexer) peekBy(n int) string {
	runes := make([]rune, n)
	for i := 0; i < n; i++ {
		runes[i] = l.next()
	}
	l.backupBy(n)

	return string(runes)
}

// Gets the current value of the lexer
func (l *lexer) val() string {
	return l.input[l.start:l.pos]
}

func (l *lexer) ignore() {
	l.start = l.pos
}

func (l *lexer) advance() {
	l.pos++
}

func (l *lexer) advanceBy(n int) {
	l.pos += n
}

func (l *lexer) backup() {
	l.pos--
}

func (l *lexer) backupBy(n int) {
	l.pos -= n
}

// possibly consumes a character in |valid|
func (l *lexer) accept(valid string) bool {
	if strings.ContainsRune(valid, l.next()) {
		return true
	}
	l.backup()
	return false
}

// consumes characters until one not in |valid| is found
func (l *lexer) acceptRun(valid string) int {
	var run int
	for strings.ContainsRune(valid, l.next()) {
		run++
	}
	l.backup()
	return run
}

// consume whitespace
func (l *lexer) acceptWhitespaceRun() {
	for unicode.IsSpace(l.next()) {
	}
	l.backup()
}

// consume characters until whitespace {
func (l *lexer) acceptNonWhitespaceRun() {
	for !unicode.IsSpace(l.next()) {
	}
	l.backup()
}

// peeks, returns whether or not the char is in |valid|
func (l *lexer) peekAccept(valid string) bool {
	if strings.ContainsRune(valid, l.peek()) {
		return true
	}
	return false
}

// peeks until char not in |valid|, returns number in |valid|
func (l *lexer) peekAcceptRun(valid string) int {
	var run int
	for strings.ContainsRune(valid, l.next()) {
		run++
	}
	l.backupBy(run + 1) // We consumed at all valid plus the first invalid
	return run
}

// sends an error token to the channel, then returns nil to end the state
// function loop in run()
func (l *lexer) errorf(format string, args ...interface{}) action {
	l.tokens <- token{
		tkError,
		fmt.Sprintf(format, args...),
	}

	return nil
}

// Emits a token to the client (parser), with value [start, pos), then advances
// start to pos.
//
// param	t	The type of the token to emit.
func (l *lexer) emit(t tokenType) {
	l.tokens <- token{t, l.val()}
	l.start = l.pos
}

func (l *lexer) run() {
	for state := lexCode; state != nil; {
		state = state(l)
	}
	close(l.tokens)
}

// action represents the state of the lexer as a function that returns the
// next state
type action func(l *lexer) action

func lexCode(l *lexer) action {
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

func (l *lexer) isEOF() bool {
	return l.peek() == EOF
}

func lexEOF(l *lexer) action {
	l.emit(tkEOF)
	return nil
}

func (l *lexer) isWhitespace() bool {
	return unicode.IsSpace(l.peek())
}

func lexWhitespace(l *lexer) action {
	l.acceptWhitespaceRun()
	l.ignore()
	return lexCode
}

// "/*"
func (l *lexer) isBlockComment() bool {
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

func lexBlockComment(l *lexer) action {
	var c rune

	l.advanceBy(2) // consume "/*"
	for c = l.next(); c != EOF; {
		if c == '*' {
			for c = l.next(); c == '*'; {
			}

			if c == '/' {
				l.ignore()
				return lexCode
			}
		}
	}

	return l.errorf("unterminated comment")
}

func (l *lexer) isCppStyleComment() bool {
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

func lexCppStyleComment(l *lexer) action {
	l.advanceBy(2) // consume "//"
	for c := l.next(); c != '\n' && c != EOF; {
	}

	l.ignore()
	return lexCode
}

// keyword, or {L}({L}|{D})* where
// {L} = [a-zA-Z_]
// {D} = [0-9]
func (l *lexer) isIdentifier() bool {
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

func lexIdentifier(l *lexer) action {
	l.accept("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_")

	switch id := l.val(); id {
	case "auto":
		l.emit(tkAuto)
	case "break":
		l.emit(tkBreak)
	case "case":
		l.emit(tkCase)
	case "char":
		l.emit(tkChar)
	case "const":
		l.emit(tkConst)
	case "continue":
		l.emit(tkContinue)
	case "default":
		l.emit(tkDefault)
	case "do":
		l.emit(tkDo)
	case "double":
		l.emit(tkDouble)
	case "else":
		l.emit(tkElse)
	case "enum":
		l.emit(tkEnum)
	case "extern":
		l.emit(tkExtern)
	case "float":
		l.emit(tkFloat)
	case "for":
		l.emit(tkFor)
	case "goto":
		l.emit(tkGoto)
	case "if":
		l.emit(tkIf)
	case "inline":
		l.emit(tkInline)
	case "int":
		l.emit(tkInt)
	case "long":
		l.emit(tkLong)
	case "register":
		l.emit(tkRegister)
	case "restrict":
		l.emit(tkRestrict)
	case "return":
		l.emit(tkReturn)
	case "short":
		l.emit(tkShort)
	case "signed":
		l.emit(tkSigned)
	case "sizeof":
		l.emit(tkSizeof)
	case "static":
		l.emit(tkStatic)
	case "struct":
		l.emit(tkStruct)
	case "switch":
		l.emit(tkSwitch)
	case "typedef":
		l.emit(tkTypedef)
	case "union":
		l.emit(tkUnion)
	case "unsigned":
		l.emit(tkUnsigned)
	case "void":
		l.emit(tkVoid)
	case "volatile":
		l.emit(tkVolatile)
	case "while":
		l.emit(tkWhile)
	default:
		l.emit(tkIdentifier)
	}

	return lexCode
}

// See www.quut.com/c/ANSI-C-grammar-l-2011.html
func (l *lexer) isConstant() bool {
	return l.peekAccept(DIGITS) // Doesn't catch {CP}?"'"([^'\\\n]|{ES})+"'"
}

// Lexes a number constant. Has very relaxed rules about what is actually legal
// C, because gcc can deal with all that shit. All I care about is something
// that looks number-ey, which includes, for example, the following known bugs:
//		- Octals outside of the octal range, ex. "08"
//		- No digits after [eE] or [pP], ex. "5.00e"
//		- Multiple suffixes of the same type, ex. "100lLuUfF"
func lexConstant(l *lexer) action {
	//l.emit(tokenNumber)
	//return lexInsideAction
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

	l.emit(tkConstant)
	return lexCode
}

func (l *lexer) isStringLiteral() bool {
	// TODO
	return false
}

func lexStringLiteral(l *lexer) action {
	// TODO
	return lexCode
}

// catchall for symbols, operators, etc.
func (l *lexer) isOther() bool {
	return true
}

func lexOther(l *lexer) action {
	switch c := l.next(); c {
	case '.':
		if l.peekBy(2) == ".." {
			l.advanceBy(2)
			l.emit(tkEllipsis)
		} else {
			l.emit(tkDot)
		}
	case '>':
		if l.peekBy(2) == ">=" {
			l.advanceBy(2)
			l.emit(tkRightAssign)
		} else if l.peek() == '>' {
			l.advance()
			l.emit(tkRightOp)
		} else if l.peek() == '=' {
			l.advance()
			l.emit(tkGeOp)
		} else {
			l.emit(tkGtOp)
		}
	case '<':
		if l.peekBy(2) == "<=" {
			l.advanceBy(2)
			l.emit(tkLeftAssign)
		} else if l.peek() == '<' {
			l.advance()
			l.emit(tkLeftOp)
		} else if l.peek() == '=' {
			l.advance()
			l.emit(tkLeOp)
		} else if l.peek() == '%' {
			l.advance()
			l.emit(tkLeftCurlyBracket)
		} else if l.peek() == ':' {
			l.advance()
			l.emit(tkLeftSquareBracket)
		} else {
			l.emit(tkLtOp)
		}
	case '+':
		if l.peek() == '=' {
			l.advance()
			l.emit(tkAddAssign)
		} else if l.peek() == '+' {
			l.advance()
			l.emit(tkIncOp)
		} else {
			l.emit(tkPlus)
		}
	case '-':
		if l.peek() == '=' {
			l.advance()
			l.emit(tkSubAssign)
		} else if l.peek() == '-' {
			l.advance()
			l.emit(tkDecOp)
		} else if l.peek() == '>' {
			l.advance()
			l.emit(tkPtrOp)
		} else {
			l.emit(tkMinus)
		}
	case '*':
		if l.peek() == '=' {
			l.advance()
			l.emit(tkMulAssign)
		} else {
			l.emit(tkStar)
		}
	case '/':
		if l.peek() == '=' {
			l.advance()
			l.emit(tkDivAssign)
		} else {
			l.emit(tkDiv)
		}
	case '%':
		if l.peek() == '=' {
			l.advance()
			l.emit(tkModAssign)
		} else if l.peek() == '>' {
			l.advance()
			l.emit(tkRightCurlyBracket)
		} else {
			l.emit(tkMod)
		}
	case '&':
		if l.peek() == '=' {
			l.advance()
			l.emit(tkAndAssign)
		} else if l.peek() == '&' {
			l.advance()
			l.emit(tkAndOp)
		} else {
			l.emit(tkAmpersand)
		}
	case '^':
		if l.peek() == '=' {
			l.advance()
			l.emit(tkXorAssign)
		} else {
			l.emit(tkCarrot)
		}
	case '|':
		if l.peek() == '=' {
			l.advance()
			l.emit(tkOrAssign)
		} else if l.peek() == '|' {
			l.advance()
			l.emit(tkOrOp)
		} else {
			l.emit(tkPipe)
		}
	case '=':
		if l.peek() == '=' {
			l.advance()
			l.emit(tkEqOp)
		} else {
			l.emit(tkAssign)
		}
	case '!':
		if l.peek() == '=' {
			l.advance()
			l.emit(tkNeOp)
		} else {
			l.emit(tkBang)
		}
	case ';':
		l.emit(tkSemicolon)
	case '{':
		l.emit(tkLeftCurlyBracket)
	case '}':
		l.emit(tkLeftCurlyBracket)
	case ',':
		l.emit(tkComma)
	case ':':
		if l.peek() == '>' {
			l.advance()
			l.emit(tkRightSquareBracket)
		} else {
			l.emit(tkColon)
		}
	case '(':
		l.emit(tkLeftParen)
	case ')':
		l.emit(tkRightParen)
	case '[':
		l.emit(tkLeftSquareBracket)
	case ']':
		l.emit(tkRightSquareBracket)
	case '~':
		l.emit(tkTilde)
	case '?':
		l.emit(tkQuestionMark)
	default:
		l.ignore()
	}

	return lexCode
}
