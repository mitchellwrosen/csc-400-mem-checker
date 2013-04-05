package main

import (
	//	"fmt"
	"testing"
)

func TestNext(t *testing.T) {
	l := lexer{input: "foo"}

	if c := l.next(); c != 'f' {
		t.Errorf("Expected 'f', found '%c'", c)
	}
	if c := l.next(); c != 'o' {
		t.Errorf("Expected 'o', found '%c'", c)
	}
	if c := l.next(); c != 'o' {
		t.Errorf("Expected 'o', found '%c'", c)
	}
	if c := l.next(); c != EOF {
		t.Errorf("Expected EOF, found '%c'", c)
	}
}

func TestPeek(t *testing.T) {
	l := lexer{input: "foo"}

	if c := l.peek(); c != 'f' {
		t.Errorf("Expected 'f', found '%c'", c)
	}
	if c := l.next(); c != 'f' {
		t.Errorf("Expected 'f', found '%c'", c)
	}
}

func TestBackup(t *testing.T) {
	l := lexer{input: "foo"}

	l.next()                     // consume f
	l.backup()                   // back to f
	if c := l.next(); c != 'f' { // consume f again
		t.Errorf("Expected 'f', found '%c'", c)
	}

	l.next()      // consume first o
	l.backupBy(2) // back to f
	if c := l.next(); c != 'f' {
		t.Errorf("Expected 'f', found '%c'", c)
	}
}

func TestAccept(t *testing.T) {
}
