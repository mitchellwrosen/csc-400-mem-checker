package main

import (
	"fmt"
)

type tokenType int

const (
	tkEOF tokenType = iota
	tkError

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
	tkInline
	tkInt
	tkLong
	tkRegister
	tkRestrict
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

	tkConstant // int constant, float constant

	tkStringLiteral

	tkEllipsis
	tkRightAssign
	tkLeftAssign
	tkAddAssign
	tkSubAssign
	tkMulAssign
	tkDivAssign
	tkModAssign
	tkAndAssign
	tkXorAssign
	tkOrAssign
	tkRightOp
	tkLeftOp
	tkIncOp
	tkDecOp
	tkPtrOp
	tkAndOp
	tkOrOp
	tkLeOp
	tkGeOp
	tkEqOp
	tkNeOp
	tkSemicolon
	tkLeftCurlyBracket
	tkRightCurlyBracket
	tkComma
	tkColon
	tkAssign
	tkLeftParen
	tkRightParen
	tkLeftSquareBracket
	tkRightSquareBracket
	tkDot
	tkAmpersand
	tkBang
	tkTilde
	tkMinus
	tkPlus
	tkStar
	tkDiv
	tkMod
	tkLtOp
	tkGtOp
	tkCarrot
	tkPipe
	tkQuestionMark
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
