package main

import (
	"fmt"
)

type TokenType int

const (
	tkEOF TokenType = iota
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

func (t TokenType) String() string {
	switch t {
	case tkEOF:
		return "tkEOF"
	case tkError:
		return "tkError"
	case tkAuto:
		return "tkAuto"
	case tkBreak:
		return "tkBreak"
	case tkCase:
		return "tkCase"
	case tkChar:
		return "tkChar"
	case tkConst:
		return "tkConst"
	case tkContinue:
		return "tkContinue"
	case tkDefault:
		return "tkDefault"
	case tkDo:
		return "tkDo"
	case tkDouble:
		return "tkDouble"
	case tkElse:
		return "tkElse"
	case tkEnum:
		return "tkEnum"
	case tkExtern:
		return "tkExtern"
	case tkFloat:
		return "tkFloat"
	case tkFor:
		return "tkFor"
	case tkGoto:
		return "tkGoto"
	case tkIf:
		return "tkIf"
	case tkInline:
		return "tkInline"
	case tkInt:
		return "tkInt"
	case tkLong:
		return "tkLong"
	case tkRegister:
		return "tkRegister"
	case tkRestrict:
		return "tkRestrict"
	case tkReturn:
		return "tkReturn"
	case tkShort:
		return "tkShort"
	case tkSigned:
		return "tkSigned"
	case tkSizeof:
		return "tkSizeof"
	case tkStatic:
		return "tkStatic"
	case tkStruct:
		return "tkStruct"
	case tkSwitch:
		return "tkSwitch"
	case tkTypedef:
		return "tkTypedef"
	case tkUnion:
		return "tkUnion"
	case tkUnsigned:
		return "tkUnsigned"
	case tkVoid:
		return "tkVoid"
	case tkVolatile:
		return "tkVolatile"
	case tkWhile:
		return "tkWhile"
	case tkIdentifier:
		return "tkIdentifier"
	case tkConstant:
		return "tkConstant"
	case tkStringLiteral:
		return "tkStringLiteral"
	case tkEllipsis:
		return "tkEllipsis"
	case tkRightAssign:
		return "tkRightAssign"
	case tkLeftAssign:
		return "tkLeftAssign"
	case tkAddAssign:
		return "tkAddAssign"
	case tkSubAssign:
		return "tkSubAssign"
	case tkMulAssign:
		return "tkMulAssign"
	case tkDivAssign:
		return "tkDivAssign"
	case tkModAssign:
		return "tkModAssign"
	case tkAndAssign:
		return "tkAndAssign"
	case tkXorAssign:
		return "tkXorAssign"
	case tkOrAssign:
		return "tkOrAssign"
	case tkRightOp:
		return "tkRightOp"
	case tkLeftOp:
		return "tkLeftOp"
	case tkIncOp:
		return "tkIncOp"
	case tkDecOp:
		return "tkDecOp"
	case tkPtrOp:
		return "tkPtrOp"
	case tkAndOp:
		return "tkAndOp"
	case tkOrOp:
		return "tkOrOp"
	case tkLeOp:
		return "tkLeOp"
	case tkGeOp:
		return "tkGeOp"
	case tkEqOp:
		return "tkEqOp"
	case tkNeOp:
		return "tkNeOp"
	case tkSemicolon:
		return "tkSemicolon"
	case tkLeftCurlyBracket:
		return "tkLeftCurlyBracket"
	case tkRightCurlyBracket:
		return "tkRightCurlyBracket"
	case tkComma:
		return "tkComma"
	case tkColon:
		return "tkColon"
	case tkAssign:
		return "tkAssign"
	case tkLeftParen:
		return "tkLeftParen"
	case tkRightParen:
		return "tkRightParen"
	case tkLeftSquareBracket:
		return "tkLeftSquareBracket"
	case tkRightSquareBracket:
		return "tkRightSquareBracket"
	case tkDot:
		return "tkDot"
	case tkAmpersand:
		return "tkAmpersand"
	case tkBang:
		return "tkBang"
	case tkTilde:
		return "tkTilde"
	case tkMinus:
		return "tkMinus"
	case tkPlus:
		return "tkPlus"
	case tkStar:
		return "tkStar"
	case tkDiv:
		return "tkDiv"
	case tkMod:
		return "tkMod"
	case tkLtOp:
		return "tkLtOp"
	case tkGtOp:
		return "tkGtOp"
	case tkCarrot:
		return "tkCarrot"
	case tkPipe:
		return "tkPipe"
	case tkQuestionMark:
		return "tkQuestionMark"
	}

	panic("NOTREACHED")
}

type Token struct {
	typ TokenType
	val string
}

func (t Token) String() string {
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
