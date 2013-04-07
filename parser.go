package main

import (
	"fmt"
)

type symbol int

const (
	symPrimaryExpression symbol = iota
	symConstant
	symEnumerationConstant
	symString
	symGenericSelection
	symGenericAssocList
	symGenericAssociation
	symPostfixExpression
	symArgumentExpressionList
	symUnaryExpression
	symUnaryOperator
	symCastExpression
	symMultiplicativeExpression
	symAdditiveExpression
	symShiftExpression
	symRelationalExpression
	symEqualityExpression
	symAndExpression
	symExclusiveOrExpression
	symInclusiveOrExpression
	symLogicalAndExpression
	symLogicalOrExpression
	symConditionalExpression
	symAssignmentExpression
	symAssignmentOperator
	symExpression
	symConstantExpression
	symDeclaration
	symDeclarationSpecifiers
	symInitDeclaratorList
	symInitDeclarator
	symStorageClassSpecifier
	symTypeSpecifier
	symStructOrUnionSpecifier
	symStructOrUnion
	symStructDeclarationList
	symStructDeclaration
	symSpecifierQualifierList
	symStructDeclaratorList
	symStructDeclarator
	symEnumSpecifier
	symEnumeratorList
	symEnumerator
	symAtomicTypeSpecifier
	symTypeQualifier
	symFunctionSpecifier
	symAlignmentSpecifier
	symDeclarator
	symDirectDeclarator
	symPointer
	symTypeQualifierList
	symParameterTypeList
	symParameterList
	symParameterDeclaration
	symIdentifierList
	symTypeName
	symAbstractDeclarator
	symDirectAbstractDeclarator
	symInitializer
	symInitializerList
	symDesignation
	symDesignatorList
	symDesignator
	symStaticAssertDeclaration
	symStatement
	symLabeledStatement
	symCompoundStatement
	symBlockItemList
	symBlockItem
	symExpressionStatement
	symSelectionStatement
	symIterationStatement
	symJumpStatement
	symTranslationUnit
	symExternalDeclaration
	symFunctionDefinition
	symDeclarationList
)

type parser struct {
	l       *lexer
	tokenCh chan token

	tk token // One token lookahead
}

// Consumes the next token if the current matches |typ|, throws an error
// otherwise.
func (p *parser) match(typ tokenType) error {
	if p.tkType() == typ {
		p.tk = <-p.tokenCh
		return nil
	}

	return fmt.Errorf("Expected %s, got %s", typ, p.tk.typ)
}

func (p *parser) tkType() tokenType {
	return p.tk.typ
}

func (p *parser) first(sym symbol) bool {
	tkType := p.tkType()
	switch sym {
	case symPrimaryExpression:
		return tkType == tkIdentifier ||
			p.first(symConstant) ||
			p.first(symString) ||
			tkType == tkLeftParen ||
			p.first(symGenericSelection)
	case symConstant:
		return tkType == tkConstant ||
			p.first(symEnumerationConstant)
	case symEnumerationConstant:
		return tkType == tkIdentifier
	case symString:
		return tkType == tkStringLiteral
		// || p.tkType == tkFuncName
	case symGenericSelection:
		//return tkType == tkGeneric
		return false
	case symGenericAssocList:
		return p.first(symGenericAssociation)
	case symGenericAssociation:
		return p.first(symTypeName) ||
			tkType == tkDefault
	case symPostfixExpression:
		return p.first(symPrimaryExpression) ||
			tkType == tkLeftParen
	case symArgumentExpressionList:
		return p.first(symAssignmentExpression)
	case symUnaryExpression:
		return p.first(symPostfixExpression) ||
			tkType == tkIncOp ||
			tkType == tkDecOp ||
			p.first(symUnaryOperator) ||
			tkType == tkSizeof
		// || tkType == tkAlignof
	case symUnaryOperator:
		return tkType == tkAmpersand ||
			tkType == tkStar ||
			tkType == tkPlus ||
			tkType == tkMinus ||
			tkType == tkTilde ||
			tkType == tkBang
	case symCastExpression:
		return p.first(symUnaryExpression) ||
			tkType == tkLeftParen
	case symMultiplicativeExpression:
		return p.first(symCastExpression)
	case symAdditiveExpression:
		return p.first(symMultiplicativeExpression)
	case symShiftExpression:
		return p.first(symAdditiveExpression)
	case symRelationalExpression:
		return p.first(symShiftExpression)
	case symEqualityExpression:
		return p.first(symRelationalExpression)
	case symAndExpression:
		return p.first(symEqualityExpression)
	case symExclusiveOrExpression:
		return p.first(symAndExpression)
	case symInclusiveOrExpression:
		return p.first(symExclusiveOrExpression)
	case symLogicalAndExpression:
		return p.first(symInclusiveOrExpression)
	case symLogicalOrExpression:
		return p.first(symLogicalAndExpression)
	case symConditionalExpression:
		return p.first(symLogicalOrExpression)
	case symAssignmentExpression:
		return p.first(symConditionalExpression) ||
			p.first(symUnaryExpression)
	case symAssignmentOperator:
		return tkType == tkAssign ||
			tkType == tkMulAssign ||
			tkType == tkDivAssign ||
			tkType == tkModAssign ||
			tkType == tkAddAssign ||
			tkType == tkSubAssign ||
			tkType == tkLeftAssign ||
			tkType == tkRightAssign ||
			tkType == tkAndAssign ||
			tkType == tkXorAssign ||
			tkType == tkOrAssign
	case symExpression:
		return p.first(symAssignmentExpression)
	case symConstantExpression:
		return p.first(symConditionalExpression)
	case symDeclaration:
		return p.first(symDeclarationSpecifiers) ||
			p.first(symStaticAssertDeclaration)
	case symDeclarationSpecifiers:
		return p.first(symStorageClassSpecifier) ||
			p.first(symTypeSpecifier) ||
			p.first(symFunctionSpecifier) ||
			p.first(symAlignmentSpecifier)
	case symInitDeclaratorList:
		return p.first(symInitDeclarator)
	case symInitDeclarator:
		return p.first(symDeclarator)
	case symStorageClassSpecifier:
		return tkType == tkTypedef ||
			tkType == tkExtern ||
			tkType == tkStatic ||
			//tkType == tkThreadLocal ||
			tkType == tkAuto ||
			tkType == tkRegister
	case symTypeSpecifier:
		return tkType == tkVoid ||
			tkType == tkChar ||
			tkType == tkShort ||
			tkType == tkInt ||
			tkType == tkLong ||
			tkType == tkFloat ||
			tkType == tkDouble ||
			tkType == tkSigned ||
			tkType == tkUnsigned ||
			//tkType == tkBool ||
			//tkType == tkComplex ||
			//tkType == tkImaginary ||
			p.first(symAtomicTypeSpecifier) ||
			p.first(symStructOrUnionSpecifier) ||
			p.first(symEnumSpecifier)
		// || tkType == tkTypedefName // after it has been defined as such
	case symStructOrUnionSpecifier:
		return p.first(symStructOrUnion)
	case symStructOrUnion:
		return tkType == tkStruct ||
			tkType == tkUnion
	case symStructDeclarationList:
		return p.first(symStructDeclaration)
	case symStructDeclaration:
		return p.first(symSpecifierQualifierList) ||
			p.first(symStaticAssertDeclaration)
	case symSpecifierQualifierList:
		return p.first(symTypeSpecifier) ||
			p.first(symTypeQualifier)
	case symStructDeclaratorList:
		return p.first(symStructDeclarator)
	case symStructDeclarator:
		return tkType == tkColon ||
			p.first(symDeclarator)
	case symEnumSpecifier:
		return tkType == tkEnum
	case symEnumeratorList:
		return p.first(symEnumerator)
	case symEnumerator:
		return p.first(symEnumerationConstant)
	case symAtomicTypeSpecifier:
		//return tkType == tkAtomic
		return false
	case symTypeQualifier:
		return tkType == tkConst ||
			tkType == tkRestrict ||
			tkType == tkVolatile
		// || tkType == tkAtomic
	case symFunctionSpecifier:
		return tkType == tkInline
		// || tkType == tkNoReturn
	case symAlignmentSpecifier:
		//return tkType == tkAlignAs
		return false
	case symDeclarator:
		return p.first(symPointer) ||
			p.first(symDirectDeclarator)
	case symDirectDeclarator:
		return tkType == tkIdentifier ||
			tkType == tkLeftParen ||
			p.first(symDirectDeclarator)
	case symPointer:
		return tkType == tkStar
	case symTypeQualifierList:
		return p.first(symTypeQualifier)
	case symParameterTypeList:
		return p.first(symParameterList)
	case symParameterList:
		return p.first(symParameterDeclaration)
	case symParameterDeclaration:
		return p.first(symDeclarationSpecifiers)
	case symIdentifierList:
		return tkType == tkIdentifier
	case symTypeName:
		return p.first(symSpecifierQualifierList)
	case symAbstractDeclarator:
		return p.first(symPointer) ||
			p.first(symDirectAbstractDeclarator)
	case symDirectAbstractDeclarator:
		return tkType == tkLeftParen ||
			tkType == tkLeftSquareBracket ||
			p.first(symDirectAbstractDeclarator)
	case symInitializer:
		return tkType == tkLeftCurlyBracket ||
			p.first(symAssignmentExpression)
	case symInitializerList:
		return p.first(symDesignation) ||
			p.first(symInitializer)
	case symDesignation:
		return p.first(symDesignatorList)
	case symDesignatorList:
		return p.first(symDesignator)
	case symDesignator:
		return tkType == tkLeftSquareBracket ||
			tkType == tkDot
	case symStaticAssertDeclaration:
		//return tkType == tkStaticAssert
		return false
	case symStatement:
		return p.first(symLabeledStatement) ||
			p.first(symCompoundStatement) ||
			p.first(symExpressionStatement) ||
			p.first(symSelectionStatement) ||
			p.first(symIterationStatement) ||
			p.first(symJumpStatement)
	case symLabeledStatement:
		return tkType == tkIdentifier ||
			tkType == tkCase ||
			tkType == tkDefault
	case symCompoundStatement:
		return tkType == tkLeftCurlyBracket
	case symBlockItemList:
		return p.first(symBlockItem)
	case symBlockItem:
		return p.first(symDeclaration) ||
			p.first(symStatement)
	case symExpressionStatement:
		return tkType == tkSemicolon ||
			p.first(symExpression)
	case symSelectionStatement:
		return tkType == tkIf ||
			tkType == tkSwitch
	case symIterationStatement:
		return tkType == tkWhile ||
			tkType == tkDo ||
			tkType == tkFor
	case symJumpStatement:
		return tkType == tkGoto ||
			tkType == tkContinue ||
			tkType == tkBreak ||
			tkType == tkReturn
	case symTranslationUnit:
		return p.first(symExternalDeclaration)
	case symExternalDeclaration:
		return p.first(symFunctionDefinition) ||
			p.first(symDeclaration)
	case symFunctionDefinition:
		return p.first(symDeclarationSpecifiers)
	case symDeclarationList:
		return p.first(symDeclaration)
	}
	panic("NOTREACHED")
}
