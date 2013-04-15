package main

import (
	. "launchpad.net/gocheck"
	"strings"
	"testing"
)

// Gocheck boiler plate
func Test(t *testing.T) { TestingT(t) }

type S struct{}

var _ = Suite(&S{})

func (s *S) TestNext(c *C) {
	l := Lexer{input: "foo"}

	c.Check(l.next(), Equals, 'f')
	c.Check(l.next(), Equals, 'o')
	c.Check(l.next(), Equals, 'o')
	c.Check(l.next(), Equals, int32(-1))
}

func (s *S) TestPeek(c *C) {
	l := Lexer{input: "foo"}

	c.Check(l.peek(), Equals, 'f')
	c.Check(l.next(), Equals, 'f')
}

func (s *S) TestBackup(c *C) {
	l := Lexer{input: "foo"}

	l.next()   // consume f
	l.backup() // back to f
	c.Check(l.next(), Equals, 'f')

	l.next()      // consume first o
	l.backupBy(2) // back to f
	c.Check(l.next(), Equals, 'f')
}

func (s *S) TestAccept(c *C) {
	l := Lexer{input: "foofoobar"}

	c.Check(l.accept("f"), Equals, true) // consume f
	c.Check(l.peek(), Equals, 'o')

	l.backup()        // back to f
	l.acceptRun("fo") // advance to bar
	c.Check(l.peek(), Equals, 'b')

	l.backupBy(6) // back to first f
	c.Check(l.peekAccept("f"), Equals, true)
	c.Check(l.peek(), Equals, 'f')
	c.Check(l.peekAcceptRun("fo"), Equals, 6)
	c.Check(l.peek(), Equals, 'f')
}

func (s *S) TestEmit(c *C) {
	// Artificially put lexer in a ready-to-emit state
	tokens := make([]Token, 0, 10)
	l := Lexer{
		input:  "012345",
		start:  0,
		pos:    5,
		tokens: tokens,
	}

	l.appendToken(tkConstant)
	c.Check(l.start, Equals, l.pos)

	c.Assert(len(l.tokens), Equals, 1)
	c.Check(l.tokens[0].typ, Equals, tkConstant)
	c.Check(l.tokens[0].val, Equals, "01234")

	c.Check(l.peek(), Equals, '5')
}

func (s *S) TestPredicates(c *C) {
	l := Lexer{input: "/* foo */"}
	c.Check(l.isBlockComment(), Equals, true)

	l.input = "// foo"
	c.Check(l.isCppStyleComment(), Equals, true)

	l.input = "int"
	c.Check(l.isIdentifier(), Equals, true)
	l.input = "integer123"
	c.Check(l.isIdentifier(), Equals, true)
	l.input = "int_eger"
	c.Check(l.isIdentifier(), Equals, true)

	l.input = "12345"
	c.Check(l.isConstant(), Equals, true)

	// TODO: more
}

func (s *S) TestLexComments(c *C) {
	tks := Lex("/* foo */\n// bar")

	c.Assert(len(tks), Equals, 1)
	c.Check(tks[0].typ, Equals, tkEOF)
}

func (s *S) TestLexIdentifier(c *C) {
	keywords := []string{"auto", "break", "case", "char", "const", "continue",
		"default", "do", "double",
		"else", "enum", "extern", "float", "for", "goto", "if",
		"inline", "int", "long",
		"register", "restrict", "return", "short", "signed",
		"sizeof", "static",
		"struct", "switch", "typedef", "union", "unsigned",
		"void", "volatile", "while"}
	keywordTypes := []TokenType{tkAuto, tkBreak, tkCase, tkChar, tkConst,
		tkContinue, tkDefault, tkDo, tkDouble, tkElse, tkEnum, tkExtern,
		tkFloat, tkFor, tkGoto, tkIf, tkInline, tkInt, tkLong, tkRegister,
		tkRestrict, tkReturn, tkShort, tkSigned, tkSizeof, tkStatic, tkStruct,
		tkSwitch, tkTypedef, tkUnion, tkUnsigned, tkVoid, tkVolatile, tkWhile,
	}
	identifiers := []string{"foo", "f00", "foo_bar", "__foo__"}
	input := strings.Join(append(keywords, identifiers...), " ")

	tks := Lex(input)

	c.Assert(len(tks), Equals, len(keywords)+len(identifiers)+1)
	for i, kw := range keywordTypes {
		c.Check(tks[i].typ, Equals, kw)
	}

	shift := len(keywords)
	for i, id := range identifiers {
		c.Check(tks[shift+i].typ, Equals, tkIdentifier)
		c.Check(tks[shift+i].val, Equals, id)
	}

	c.Check(tks[len(keywords)+len(identifiers)].typ, Equals, tkEOF)
}

func (s *S) TestConstant(c *C) {
	// Integer constants
	input := []string{"0xa", "0xAu", "01", "500L"}
	tks := Lex(strings.Join(input, " "))

	c.Assert(len(tks), Equals, 5)
	for i := 0; i < 4; i++ {
		c.Check(tks[i].typ, Equals, tkConstant)
		c.Check(tks[i].val, Equals, input[i])
	}
	c.Check(tks[4].typ, Equals, tkEOF)

	// Floating point constants
	input = []string{"5e4", "5e-4", "5e+4", "5e4f", "5e4l", "0.5", ".5",
		".5e-4l", "0xF.Fp4"}
	tks = Lex(strings.Join(input, " "))

	c.Assert(len(tks), Equals, 10)
	for i := 0; i < 9; i++ {
		c.Check(tks[i].typ, Equals, tkConstant)
		c.Check(tks[i].val, Equals, input[i])
	}
	c.Check(tks[9].typ, Equals, tkEOF)
}
