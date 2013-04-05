package main

import (
	"fmt"
)

type tokenType int

const (
	tkEOF tokenType = iota
	tkError

	tkComment

	tkIdentifier

	tkConstant
)

type token struct {
	typ tokenType
	val string
}

func (t token) String() string {
	switch t.typ {
	case tkEOF:
		return "EOF"
	case tkError:
		return t.val
	}

	if len(t.val) > 10 {
		return fmt.Sprintf("%.10q...", t.val)
	}
	return fmt.Sprintf("%q", t.val)
}
