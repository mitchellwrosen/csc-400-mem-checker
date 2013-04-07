package main

import (
	. "launchpad.net/gocheck"
	"strings"
	"testing"
	"time"
)

// Gocheck boiler plate
func Test(t *testing.T) { TestingT(t) }

type S struct{}

var _ = Suite(&S{})

// Helper token receiver that times out after 1 second
func receive(ch chan token) (token, bool) {
	select {
	case tk := <-ch:
		return tk, true
	case <-time.After(time.Second):
		return token{}, false
	}
	panic("NOTREACHED")
}

func (s *S) TestNext(c *C) {
	l := lexer{input: "foo"}

	c.Check(l.next(), Equals, 'f')
	c.Check(l.next(), Equals, 'o')
	c.Check(l.next(), Equals, 'o')
	c.Check(l.next(), Equals, int32(-1))
}

func (s *S) TestPeek(c *C) {
	l := lexer{input: "foo"}

	c.Check(l.peek(), Equals, 'f')
	c.Check(l.next(), Equals, 'f')
}

func (s *S) TestBackup(c *C) {
	l := lexer{input: "foo"}

	l.next()   // consume f
	l.backup() // back to f
	c.Check(l.next(), Equals, 'f')

	l.next()      // consume first o
	l.backupBy(2) // back to f
	c.Check(l.next(), Equals, 'f')
}

func (s *S) TestAccept(c *C) {
	l := lexer{input: "foofoobar"}

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
	client := make(chan token, 2)
	l := lexer{
		input:  "012345",
		start:  0,
		pos:    5,
		tokens: client,
	}

	l.emit(tkConstant)
	c.Check(l.start, Equals, l.pos)

	if tk, ok := receive(client); ok {
		c.Check(tk.typ, Equals, tkConstant)
		c.Check(tk.val, Equals, "01234")
	} else {
		c.Fatalf("timed out")
	}

	c.Check(l.peek(), Equals, '5')
}

func (s *S) TestPredicates(c *C) {
	l := lexer{input: "/* foo */"}
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
	_, tokenCh := lex("/* foo */\n// bar")
	if tk, ok := receive(tokenCh); ok {
		c.Check(tk.typ, Equals, tkEOF)
	} else {
		c.Fatalf("timed out")
	}
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
	keywordTypes := []tokenType{tkAuto, tkBreak, tkCase, tkChar, tkConst,
		tkContinue, tkDefault, tkDo, tkDouble, tkElse, tkEnum, tkExtern,
		tkFloat, tkFor, tkGoto, tkIf, tkInline, tkInt, tkLong, tkRegister,
		tkRestrict, tkReturn, tkShort, tkSigned, tkSizeof, tkStatic, tkStruct,
		tkSwitch, tkTypedef, tkUnion, tkUnsigned, tkVoid, tkVolatile, tkWhile,
	}
	identifiers := []string{"foo", "f00", "foo_bar", "__foo__"}
	input := strings.Join(append(keywords, identifiers...), " ")

	_, tokenCh := lex(input)

	for _, kw := range keywordTypes {
		if tk, ok := receive(tokenCh); ok {
			c.Check(tk.typ, Equals, kw)
		} else {
			c.Fatalf("timed out")
		}
	}

	for _, id := range identifiers {
		if tk, ok := receive(tokenCh); ok {
			c.Check(tk.typ, Equals, tkIdentifier)
			c.Check(tk.val, Equals, id)
		} else {
			c.Fatalf("timed out")
		}
	}
}

func (s *S) TestConstant(c *C) {
	// Integer constants
	input := "0xa 0xAu 01 500L"
	_, tokenCh := lex(input)
	for i := 0; i < len(strings.Split(input, " ")); i++ {
		if tk, ok := receive(tokenCh); ok {
			c.Check(tk.typ, Equals, tkConstant)
		} else {
			c.Fatalf("timed out")
		}
	}

	// Floating point constants
	input = "5e4 5e-4 5e+4 5e4f 5e4l 0.5 .5 .5e-4l 0xF.Fp4"
	_, tokenCh = lex(input)
	for i := 0; i < len(strings.Split(input, " ")); i++ {
		if tk, ok := receive(tokenCh); ok {
			c.Check(tk.typ, Equals, tkConstant)
		} else {
			c.Fatalf("timed out")
		}
	}
}
