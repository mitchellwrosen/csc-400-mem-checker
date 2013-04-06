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

func isLetter(c rune) bool {
	return (c >= 'a' && c <= 'z') ||
		(c >= 'A' && c <= 'Z')
}

func isDigit(c rune) bool {
	return c >= '0' && c <= '9'
}

func isAlphaNum(c rune) bool {
	return isLetter(c) && isDigit(c)
}

type lexer struct {
	name   string     // used only for error reports
	input  string     // the string being scanned
	start  int        // start position of this token
	pos    int        // current position in the input
	tokens chan token // channel of scanned token
}

func lex(name, input string) (*lexer, chan token) {
	l := &lexer{
		name:   name,
		input:  input,
		tokens: make(chan token, 10),
	}

	go l.run()
	return l, l.tokens
}

func (l *lexer) next() rune {
	if l.pos >= len(l.input) {
		return EOF
	}

	r := l.input[l.pos]
	l.pos++
	return rune(r)
}

func (l *lexer) peek() rune {
	if l.pos >= len(l.input) {
		return EOF
	}
	return rune(l.input[l.pos])
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

// consume [a-fA-F0-9]
func (l *lexer) acceptHexDigit() bool {
	return l.accept(HEX_DIGITS)
}

// consumes characters until one not in |valid| is found
func (l *lexer) acceptRun(valid string) {
	for strings.ContainsRune(valid, l.next()) {
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

func (l *lexer) peekAcceptDigit() bool {
	return l.peekAccept(DIGITS)
}

// peeks until char not in |valid|, returns number in |valid|
func (l *lexer) peekAcceptRun(valid string) int {
	var numValid int
	for strings.ContainsRune(valid, l.next()) {
		numValid++
	}
	l.backupBy(numValid + 1) // We consumed at all valid plus the first invalid
	return numValid
}

// sends an error token to the channel, then returns nil to end the state
// function loop in run()
func (l *lexer) errorf(format string, args ...interface{}) stateFun {
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

// stateFun represents the state of the lexer as a function that returns the
// next state
type stateFun func(l *lexer) stateFun

func lexCode(l *lexer) stateFun {
	if l.isComment() {
		return lexComment
	}

	if l.isIdentifier() {
		return lexIdentifier
	}

	if l.isConstant1() {
		return lexConstant1
	}

	//if l.isStringLiteral() {
	//return lexStringLiteral
	//}

	// TODO: fix this
	return lexCode
}

// "/*"
func (l *lexer) isComment() bool {
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

func lexComment(l *lexer) stateFun {
	// TODO
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

func lexIdentifier(l *lexer) stateFun {
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
	case "int":
		l.emit(tkInt)
	case "long":
		l.emit(tkLong)
	case "register":
		l.emit(tkRegister)
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

// 0[xX]{H}+{IS}?
// 0{D}+{IS}?
// {D}+{IS}?
// L?'(\\.|[^\\'])+'
// {D}+{E}{FS}?	
// {D}*"."{D}+({E})?{FS}?
// {D}+"."{D}*({E})?{FS}?
// where {D}  = [0-9]
//	     {H}  = [a-zA-Z0-9]
//		 {E}  = [Ee][+-]?{D}+
//       {FS} = (f|F|l|L)
//       {IS} = (u|U|l|L)*
func (l *lexer) isConstant() bool {
	// matches all but the one I don't understand (4th from the top)
	return l.peekAcceptDigit()
}

// 0[xX]{H}+{IS}?
func lexConstant1(l *lexer) stateFun {
	l.next() // discard 0
	l.next() // discard [xX]

	digits := "0123456789abcdefABCDEF"
	if !l.accept(digits) {
		return l.errorf("incomplete CONSTANT")
	}
	l.acceptRun(digits)

	chars := "uUlL"
	l.accept(chars)

	// regex fully parsed - look for invalid suffix (an alphanum)
	if next := l.peek(); isAlphaNum(next) {
		l.ignore()                 // discard the entire constant
		l.acceptNonWhitespaceRun() // get the invalid suffix to report
		return l.errorf("invalid suffix \"%s\" on hex constant", l.val())
	}

	l.emit(tkConstant)

	return lexCode
}

// {D}+{IS}?
func (l *lexer) isConstant2() bool {

}

//func lexText(l *lexer) stateFun {
//for {
//if strings.HasPrefix(l.input[l.pos:], leftMeta) {
//if l.pos > l.start {
//l.emit(tokenText)
//}
//return lexLeftMeta // Next state.
//}
//if l.next() == eof {
//break
//}
//}

//// Correctly reached EOF.
//if l.pos > l.start {
//l.emit(tokenText)
//}
//l.emit(tokenEOF)
//return nil
//}

//func lexLeftMeta(l *lexer) stateFun {
//l.pos += len(leftMeta)
//l.emit(tokenLeftMeta)
//return lexInsideAction
//}

//func lexRightMeta(l *lexer) stateFun {
//l.pos += len(rightMeta)
//l.emit(tokenRightMeta)
//return lexText
//}

//func lexInsideAction(l *lexer) stateFun {
//// either number, quoted string, or identifier
//// spaces separate and are ignored
//// pipe symbols separate and are emitted
//for {
//if strings.HasPrefix(l.input[l.pos:], rightMeta) {
//return lexRightMeta
//}

//r := l.next()
//switch r {
//case r == eof || r == '\n':
//return l.errorf("unclosed action")
//case isSpace(r):
//l.ignore()
//case r == '|':
//l.emit(tokenPipe)
//case r == '"':
//return lexQuote
//case r == '`':
//return lexRawQuote
//case r == '+' || r == '-' || 'O' <= r && r <= '9':
//l.backup()
//return lexNumber
//case isAlphaNumeric(r):
//l.backup()
//return lexIdentifier
//}

//}
//}

//func lexNumber(l *lexer) stateFun {
//l.accept("+-") // Optional leading sign

//digits := "0123456789"
//if l.accept("0") && l.accept("xX") {
//digits = "0123456789abcdefABCDEF"
//}

//l.acceptRun(digits)
//if l.accept(".") {
//l.acceptRun(digits)
//}

//if l.accept("eE") {
//l.accept("+-")
//l.acceptRun("0123456789")
//}

//l.emit(tokenNumber)
//return lexInsideAction
//}
