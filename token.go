package main

import (
	"fmt"
)

type tokenType int

const (
	tkEOF tokenType = iota
	tkError

	tkComment

	tkAuto
	tkBreak
	tkCase
	tkChar
	tkConst
	tkContinue
	tkDefault
	tkDo
	tkDouble
	tkElse
	tkEnum
	tkExtern
	tkFloat
	tkFor
	tkGoto
	tkIf
	tkInt
	tkLong
	tkRegister
	tkReturn
	tkShort
	tkSigned
	tkSizeof
	tkStatic
	tkStruct
	tkSwitch
	tkTypedef
	tkUnion
	tkUnsigned
	tkVoid
	tkVolatile
	tkWhile

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
