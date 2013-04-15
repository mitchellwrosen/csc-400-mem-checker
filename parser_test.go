package main

import (
	. "launchpad.net/gocheck"
	//"testing"
)

// Helper functions ////////////////////////////////////////////////////////////

func newParser(input string) *Parser {
	return &Parser{tokens: Lex(input)}
}

func matchTokens(c *C, p *Parser, tks ...TokenType) {
	for _, tk := range tks {
		c.Check(matches(tk)(p), Equals, nil)
	}
}

func parseSemicolon(p *Parser) error {
	return matches(tkSemicolon)(p)
}

func parseAmpersand(p *Parser) error {
	return matches(tkAmpersand)(p)
}

// Higher order parser methods /////////////////////////////////////////////////

func (s *S) TestMatches(c *C) {
	p := newParser("i = 4;")
	tks := []TokenType{
		tkIdentifier,
		tkAssign,
		tkConstant,
		tkSemicolon,
		tkEOF,
	}
	matchTokens(c, p, tks...)
}

func (s *S) TestOptional(c *C) {
	p := newParser("; foo")
	c.Check(optional(parseSemicolon)(p), IsNil) // consume ';'
	c.Check(optional(parseSemicolon)(p), IsNil) // don't consume foo
	matchTokens(c, p, tkIdentifier)             // make sure foo is next
}

func (s *S) TestZeroOrMore(c *C) {
	p := newParser(";;foo")
	c.Check(zeroOrMore(parseSemicolon)(p), IsNil) // consume ';;'
	c.Check(zeroOrMore(parseSemicolon)(p), IsNil) // don't consume foo
	matchTokens(c, p, tkIdentifier)               // make sure foo is next
}

func (s *S) TestOneOrMore(c *C) {
	p := newParser(";;foo")
	c.Check(oneOrMore(parseSemicolon)(p), IsNil) // consume ';;'
	matchTokens(c, p, tkIdentifier)

	p = newParser("foo")
	c.Check(oneOrMore(parseSemicolon)(p), NotNil) // error on 'foo'
}

func (s *S) TestPercent(c *C) {
	p := newParser(";foo")
	c.Check(percent(parseSemicolon)(p), IsNil) // consume ';'
	matchTokens(c, p, tkIdentifier)

	p = newParser(";,;,;,;foo")
	c.Check(percent(parseSemicolon)(p), IsNil) // consume up to 'foo'
	matchTokens(c, p, tkIdentifier)

	p = newParser("foo")
	c.Check(percent(parseSemicolon)(p), NotNil) // error on 'foo'
}

func (s *S) TestAnyOf(c *C) {
	any := anyOf(
		parseSemicolon,
		parseAmpersand,
	)

	p := newParser(";foo")
	c.Check(any(p), IsNil) // consume ';'
	matchTokens(c, p, tkIdentifier)

	p = newParser("&foo")
	c.Check(any(p), IsNil) // consume '&'
	matchTokens(c, p, tkIdentifier)

	p = newParser("&foo")
	c.Check(any(p), IsNil)  // consume ';'
	c.Check(any(p), NotNil) // error on 'foo'
}

func (s *S) TestAllOf(c *C) {
	all := allOf(
		parseSemicolon,
		parseAmpersand,
	)

	p := newParser(";&foo")
	c.Check(all(p), IsNil) // consume ';&'
	matchTokens(c, p, tkIdentifier)

	p = newParser(";foo")
	c.Check(all(p), NotNil) // fail on partial match
}
