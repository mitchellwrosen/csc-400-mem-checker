package main

import (
	//	"fmt"
	. "launchpad.net/gocheck"
	"testing"
	"time"
)

// Gocheck boiler plate
func Test(t *testing.T) { TestingT(t) }

type S struct{}

var _ = Suite(&S{})

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

	select {
	case tk := <-client:
		c.Check(tk.typ, Equals, tkConstant)
		c.Check(tk.val, Equals, "01234")
	case <-time.After(time.Second):
		c.Errorf("Timed out waiting for tkConstant")
	}

	c.Check(l.peek(), Equals, '5')
}

func (s *S) TestPredicates(c *C) {
	l := lexer{input: "/* foo */"}
	c.Check(l.isComment(), Equals, true)

	l.input = "int"
	c.Check(l.isIdentifier(), Equals, true)
	l.input = "integer123"
	c.Check(l.isIdentifier(), Equals, true)
	l.input = "int_eger"
	c.Check(l.isIdentifier(), Equals, true)

	l.input = "0x"
	c.Check(l.isConstant(), Equals, true)
	l.input = "0X"
	c.Check(l.isConstant(), Equals, true)



	// TODO: more
}
