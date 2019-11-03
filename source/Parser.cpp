#include "Error.h"
#include "Parser.h"
#include "Workspace.h"

#include <cstdarg>
#include <stdio.h>
#include <string.h>

using namespace Moe;

enum FPDECL
{
	FPDECL_AllowCompoundDecl	= 0x1,		// allow comma-separated declaration of multiple variables.
	FPDECL_AllowVariadic		= 0x2,		// allow the list to end with variadic arguments (..)
	FPDECL_AllowUninitializer	= 0x4,		// allow decls to specify explicit uninitializers (n:int=---;}
	FPDECL_AllowBakedTypes		= 0x8, 		// allow unspecified generic types (aka $T)
	FPDECL_AllowBakedValues		= 0x10,		// allow types to be marked as baked constant values
	FPDECL_AllowConstants		= 0x20, 	// allow constant declarations
	FPDECL_AllowUsing			= 0x40,		// allow 'using' declarations
	FPDECL_AllowUnnamed			= 0x80,		// allow unnamed declarations (ie foo proc (:$T) )
	FPDECL_AllowShadowing		= 0x100,	// allow shadowing of variable or generic arg names

	FPDECL_None			= 0x0,
	FPDECL_All			= 0xFF,
};
MOE_DEFINE_GRF(GRFPDECL, FPDECL, u32);

enum FEXP
{
	FEXP_AllowLiteralMemberLabel = 0x1,

	FEXP_None			= 0x0,
	FEXP_All			= 0x1,
};
MOE_DEFINE_GRF(GRFEXP, FEXP, u32);

const char * PChzFromPark(PARK park)
{
	static const char * s_mpParkPChz[] =
	{
		"Error",
		"Identifier",
		"Reserved Word",
		"Nop",
		"Literal",
		"Additive Operator",
		"Multiplicative Operator",
		"Shift Operator",
		"Relational Operator",
		"LogicalAndOr Operator",
		"Assignment Operator",
		"Unary Operator",
		"Postfix Unary Operator",
		"Uninitializer",
		"Cast",
		"Array Element",		// [array, index]
		"Member Lookup",		// [struct, child]
		"Procedure Call",		// [procedure, arg0, arg1, ...]
		"Specialized Struct",
		"List",
		"Parameter List",
		"Expression List",
		"Generic Type Spec",
		"If",
		"Else",
		"Array Decl",
		"Reference Decl",
		"Qualifier Decl",
		"Procedure Reference Decl",
		"Decl",
		"Typedef",
		"Constant Decl",
		"Procedure Definition",
		"Enum Definition",
		"Struct Definition",
		"Enum Constant",
		"Variadic Argument",
		"Array Literal",
		"Argument Label",
		"Generic Decl",
		"Generic Struct Spec",
		"Type Argument",
		"Baked Value"
	};
	MOE_CASSERT(MOE_DIM(s_mpParkPChz) == PARK_Max, "missing PARK string");
	if (park == PARK_Nil)
		return "Nil";

	if ((park < PARK_Nil) | (park >= PARK_Max))
		return "Unknown PARK";

	return s_mpParkPChz[park];
}

STEXK StexkFromPark(PARK park)
{
	static const STEXK s_mpParkStexk[] = 
	{
		STEXK_None,			// PARK_Error,
		STEXK_Value,		// PARK_Identifier,
		STEXK_Value,		// PARK_ReservedWord,
		STEXK_None,			// PARK_Nop,
		STEXK_Value,		// PARK_Literal,
		STEXK_Operator,		// PARK_AdditiveOp,
		STEXK_Operator,		// PARK_MultiplicativeOp,
		STEXK_Operator,		// PARK_ShiftOp,
		STEXK_Operator,		// PARK_RelationalOp,
		STEXK_Operator,		// PARK_LogicalAndOrOp,
		STEXK_Operator,		// PARK_AssignmentOp,
		STEXK_Operator,		// PARK_UnaryOp,
		STEXK_Operator,		// PARK_PostfixUnaryOp,	// postfix increment, decrement
		STEXK_None,			// PARK_Uninitializer,
		STEXK_None,		// PARK_Cast,
		STEXK_None,		// PARK_ArrayElement,		// [array, index]
		STEXK_None,		// PARK_MemberLookup,		// [struct, child]
		STEXK_None,		// PARK_ProcedureCall,		// [procedure, arg0, arg1, ...]
		STEXK_None,		// PARK_SpecializedStruct,	// swapped in during typecheck for ProcedureCall nodes that turn out to be instantiated structs SArray(33)
		STEXK_None,		// PARK_List,				// declarations used by structs
		STEXK_None,		// PARK_ParameterList,		// comma separated declarations used by argument lists
		STEXK_None,		// PARK_ExpressionList,	// list of expressions, used by compound literals - doesn't error on rhs only values.
		STEXK_None,		// PARK_GenericTypeSpec,	// list of types to specify a generic procedure/struct instantiation
		STEXK_None,		// PARK_If,
		STEXK_None,		// PARK_Else,
		STEXK_None,		// PARK_ArrayDecl,
		STEXK_None,		// PARK_ReferenceDecl,		// used in type specification, not used for the unary address-of operator
		STEXK_None,		// PARK_QualifierDecl,
		STEXK_None,		// PARK_ProcedureReferenceDecl,
		STEXK_None,		// PARK_Decl,
		STEXK_None,		// PARK_Typedef,
		STEXK_None,		// PARK_ConstantDecl,
		STEXK_None,		// PARK_ProcedureDefinition,
		STEXK_None,		// PARK_EnumDefinition,
		STEXK_None,		// PARK_StructDefinition,
		STEXK_None,		// PARK_EnumConstant,
		STEXK_None,		// PARK_VariadicArg,
		STEXK_None,		// PARK_CompoundLiteral,	// array/struct literal
		STEXK_None,		// PARK_ArgumentLabel,
		STEXK_None,		// PARK_GenericDecl,
		STEXK_None,		// PARK_GenericStructSpec,		// 
		STEXK_None,		// PARK_TypeArgument,			// raw type, specified to a generic instantiation SFoo(:int)
		STEXK_None,		// PARK_BakedValue,
	};
}

template <typename T>
T * PSTNodeAlloc(Moe::Alloc * pAlloc, PARK park, Lexer * pLex, LexSpan lexsp)
{
	T * pStnod; 
	switch (T::s_stexk)
	{
	case STEXK_None:
		pStnod = MOE_NEW(pAlloc, T) T(t::s_stexk, park, lexsp, T::s_stexk);
	default:
		pStnod = MOE_NEW(pAlloc, T) T(park, lexsp);
	}

	pStnod->m_tok = TOK(pLex->m_tok);
	return pStnod;

	/*
	switch(T::s_stexk)
	{
	case STEXK_None:
	case STEXK_For:
	case STEXK_Decl:
	case STEXK_Enum:
	case STEXK_Struct:
	case STEXK_Proc:
	case STEXK_Expression:
	case STEXK_Operator:
	case STEXK_List:
	}*/
}

void STNode::SetChildArray(Moe::Alloc * pAlloc, STNode ** apStnodChild, size_t cpStnodChild)
{
	size_t cB = sizeof(STNode *) * cpStnodChild;
	m_apStnodChild = (STNode **)pAlloc->MOE_ALLOC(cB, MOE_ALIGN_OF(STNode *));
	m_cpStnodChild = cpStnodChild;
	memcpy(m_apStnodChild, apStnodChild, cB);
}

void STNode::SetChildArray(Moe::Alloc * pAlloc, STNode * pStnodChild)
{
	m_cpStnodChild = 1;
	size_t cB = sizeof(STNode *) * m_cpStnodChild;
	m_apStnodChild = (STNode **)pAlloc->MOE_ALLOC(cB, MOE_ALIGN_OF(STNode *));
	memcpy(m_apStnodChild, &pStnodChild, cB);
}

void SkipToToken(Lexer * pLex, TOK const * const aTok, int cTok, GRFLEXER grflexer)
{
	while (1)
	{
		bool fFound = (grflexer != FLEXER_None) && pLex->m_grflexer.FIsSet(grflexer);
		TOK tok = (TOK)pLex->m_tok;
		if (tok == TOK_Eof)
			break;

		for (int iTok = 0; !fFound && iTok < cTok; ++iTok)
		{
			fFound |= (tok == aTok[iTok]);
		}

		if (fFound)
			break;
		TokNext(pLex);
	}
}

bool FExpect(ParseContext * pParctx, Lexer * pLex, TOK tokExpected, const char * pChzInfo = nullptr, ...)
{
	if (pLex->m_tok != tokExpected)
	{
		char aB[1024] = {0};
		if (pChzInfo)
		{
			va_list ap;
			va_start(ap, pChzInfo);
#if WIN32
			vsprintf_s(aB, MOE_DIM(aB), pChzInfo, ap);
#else
			vsnprintf(aB, MOE_DIM(aB), pChzInfo, ap);
			aB[MOE_DIM(aB)-1] = 0;
#endif
		}

		auto strUnexpected = StrUnexpectedToken(pLex);
		EmitError(pParctx, LexSpan(pLex), ERRID_MissingToken, "Expected '%s' before '%s' %s", PChzFromTok(tokExpected), strUnexpected.m_pChz, aB);
		return false;
	}
	else
	{
		TokNext(pLex);
		return true;
	}
}

bool FIsEndOfStatement(Lexer * pLex)
{
	return pLex->m_tok == TOK(';') || pLex->m_tok == TOK('}') || pLex->m_tok == TOK_Eof || pLex->m_grflexer.FIsSet(FLEXER_EndOfLine);
}

void ExpectEndOfStatement(ParseContext * pParctx, Lexer * pLex, const char * pChzInfo = nullptr, ...)
{
	if (FIsEndOfStatement(pLex))
	{
		if (pLex->m_tok == TOK(';'))
		{
			TokNext(pLex);

			if (pLex->m_tok != TOK(';') && FIsEndOfStatement(pLex))
			{
				auto strUnexpected = StrUnexpectedToken(pLex);
				EmitWarning(pParctx, LexSpan(pLex), ERRID_OldCStyle, "Unnecessary c-style ';' found before '%s'", strUnexpected.m_pChz);
			}
		}
	}
	else
	{
		char aB[1024] = {0};
		if (pChzInfo)
		{
			va_list ap;
			va_start(ap, pChzInfo);
#if WIN32
			vsprintf_s(aB, MOE_DIM(aB), pChzInfo, ap);
#else
			vsnprintf(aB, MOE_DIM(aB), pChzInfo, ap);
			aB[MOE_DIM(aB)-1] = 0;
#endif
		}

		auto strUnexpected = StrUnexpectedToken(pLex);
		EmitError(pParctx, LexSpan(pLex), ERRID_ExpectedEndOfLine, "Expected end-of-line or ';' before '%s' %s", strUnexpected.m_pChz, aB);
	}
}

STValue * PStvalParseIdentifier(ParseContext * pParctx, Lexer * pLex)
{
	if (pLex->m_tok != TOK_Identifier)
		return nullptr;

	Moe::InString istrIdent = pLex->m_istr;
	if (istrIdent.FIsEmpty())
	{
		TokNext(pLex);
		return nullptr;
	}

	STValue * pStval = PSTNodeAlloc<STValue>(pParctx->m_pAlloc, PARK_Identifier, pLex, LexSpan(pLex));
	pStval->SetValue(pLex->m_istr);
	pStval->AssertValid();

	if (istrIdent.m_pChz[0] == '#')
	{
		EmitError(pParctx, LexSpan(pLex), ERRID_UnknownDirective, "Unknown directive encountered %s", istrIdent.m_pChz);
	}

	TokNext(pLex);
	return pStval;
}

// BB - should merge parseIdentifier and parseReservedWord
STValue * PStvalParseReservedWord(ParseContext * pParctx, Lexer * pLex, const char * pChzRwordExpected = nullptr)
{
	if (pLex->m_tok != TOK_Identifier || !FIsReservedWord(pLex->m_istr))
		return nullptr;

	if (pLex->m_istr == pChzRwordExpected)
	{
		EmitError(pParctx, LexSpan(pLex), ERRID_MissingRWord, "Expected %s before %s", pChzRwordExpected, pLex->m_istr.m_pChz);
		return nullptr;
	}

	STValue * pStval = PSTNodeAlloc<STValue>(pParctx->m_pAlloc, PARK_ReservedWord, pLex, LexSpan(pLex));
	pStval->SetValue(pLex->m_istr);
	pStval->AssertValid();
	pStval->m_tok = TOK(pLex->m_tok);

	TokNext(pLex);
	return pStval;
}

STNode * PStnodParsePrimaryExpression(ParseContext * pParctx, Lexer * pLex)
{
	switch(pLex->m_tok)
	{
		case TOK_Generic:
			{
				EmitError(pParctx, LexSpan(pLex), ERRID_GenericDeclNotAllowed,
					"Token '%s' unexpected outside of declaration.", PChzFromTok((TOK)pLex->m_tok));
				TokNext(pLex);
			}
		case TOK_Identifier:
			{
				auto pStval = PStvalParseIdentifier(pParctx, pLex);

				if (FConsumeToken(pLex, TOK_ColonColon))
				{
#if 1
					EWC_ASSERT(false, "Generic typespec shorthand is WIP");
#else
					SLexerLocation lexloc(pLex);
					CSTNode * pStnodSpec = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);

					pStnodSpec->m_tok = TOK_ColonColon;
					pStnodSpec->m_park = PARK_GenericTypeSpec;

					pStnodSpec->IAppendChild(pStnod);
					do
					{
						auto pStnodTin = PStnodParseTypeSpecifier(pParctx, pLex, "generic specifier", FPDECL_None);
						pStnodSpec->IAppendChild(pStnodTin);
					} while (FConsumeToken(pLex, TOK(',')));

					return pStnodSpec;
#endif
				}
					return pStnod;
			}
		case TOK_ReservedWord:
			{
				RWORD rword = RwordLookup(pLex);
				bool fIsRwordLiteral = false;
				switch (rword)
				{
					case RWORD_True:				// fall through
					case RWORD_False:				// fall through
					case RWORD_FileDirective:		// fall through
					case RWORD_LineDirective:		// fall through
					case RWORD_Null:				fIsRwordLiteral = true;	 break;
					default: break;
				}

				if (!fIsRwordLiteral)
					return nullptr;

				SLexerLocation lexloc(pLex);
				CSTNode * pStnod = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);

				pStnod->m_tok = TOK(pLex->m_tok);
				pStnod->m_park = PARK_Literal;

				CSTValue * pStval = EWC_NEW(pParctx->m_pAlloc, CSTValue) CSTValue();
				pStval->m_stvalk = STVALK_ReservedWord;
				pStval->m_rword = rword;
				pStnod->m_pTin = nullptr;

				pStnod->m_pStval = pStval;
				switch (rword)
				{
				case RWORD_True:
					{
						pStval->m_nUnsigned = 1;
						pStval->m_litkLex = LITK_Bool;
					} break;
				case RWORD_False:
					{
						pStval->m_nUnsigned = 0;
						pStval->m_litkLex = LITK_Bool;
					} break;
				case RWORD_Null:
					{
						pStval->m_nUnsigned = 0;
						pStval->m_litkLex = LITK_Null;
					} break;
				case RWORD_FileDirective:
					{
						pStval->m_stvalk = STVALK_String;
						pStval->m_litkLex = LITK_String;
						pStval->m_str = lexloc.m_strFilename;
					} break;
				case RWORD_LineDirective:
					{
						s32 iLine;
						s32 iCol;
						CalculateLinePosition(pParctx->m_pWork, &lexloc, &iLine, &iCol);

						pStval->m_litkLex = LITK_Integer;
						pStval->m_nUnsigned = iLine;
					} break;
				default:
					break;
				}

				if (pStval->m_str.FIsEmpty())
				{
					pStval->m_str = pLex->m_str;
				}


				TokNext(pLex);
				return pStnod;
			}
		case TOK_Literal:
			{
				// NOTE - Negative literals don't exist until the type checking phase folds in the unary '-' operator

				SLexerLocation lexloc(pLex);
				CSTNode * pStnod = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);

				pStnod->m_tok = TOK(pLex->m_tok);
				pStnod->m_park = PARK_Literal;

				CSTValue * pStval = EWC_NEW(pParctx->m_pAlloc, CSTValue) CSTValue();
				pStval->m_str = pLex->m_str;
				pStval->m_litkLex = pLex->m_litk;

				if (pLex->m_litk == LITK_Float)
				{
					SetFloatValue(pStval, pLex->m_g);
				}
				else if (pLex->m_litk == LITK_String)
				{
					pStval->m_stvalk = STVALK_String;
				}
				else if (pLex->m_litk == LITK_Char)
				{
					SetUnsignedIntValue(pStval, pLex->m_n);
				}
				else
				{
					SetUnsignedIntValue(pStval, pLex->m_n);
				}

				pStnod->m_pStval = pStval;

				TokNext(pLex);
				return pStnod;
			} 
		case TOK(':'): // struct literal
		case TOK('{'): // array literals
			{
				SLexerLocation lexloc(pLex);

				CSTNode * pStnodType = nullptr;

				if (pLex->m_tok == TOK(':'))
				{
					// parse type specifier
					TokNext(pLex); // consume ':'
					
					pStnodType = PStnodParseTypeSpecifier(pParctx, pLex, "compound literal", FPDECL_AllowBakedTypes | FPDECL_AllowBakedValues);

					if (!pStnodType)
					{
						ParseError(pParctx, &lexloc, ERRID_TypeSpecParseFail, "expected type specification following ':'");
						return nullptr;
					}
				}

				if (FConsumeToken(pLex, TOK('{')))
				{
					CSTNode * pStnodLit = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
					pStnodLit->m_tok = TOK(pLex->m_tok);
					pStnodLit->m_park = PARK_CompoundLiteral;

					// We're using a decl here... may need a custom structure
					auto pStdecl = pStnodLit->PStmapEnsure<CSTDecl>(pParctx->m_pAlloc);

					pStdecl->m_iStnodType = pStnodLit->IAppendChild(pStnodType);

					CSTNode * pStnodValues = PStnodParseExpressionList(pParctx, pLex, FEXP_AllowLiteralMemberLabel);
					pStdecl->m_iStnodInit = pStnodLit->IAppendChild(pStnodValues);

					FExpect(pParctx, pLex, TOK('}'), "while parsing struct/array literal");
					return pStnodLit;
				}


				CSTNode * pStnodArg = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
				pStnodArg->m_tok = TOK(':');
				pStnodArg->m_park = PARK_TypeArgument;
				pStnodArg->IAppendChild(pStnodType);


				auto pTinType = pParctx->m_pSymtab->PTinBuiltin(CSymbolTable::s_strType);
				pStnodArg->m_pTin = pTinType;
				EWC_ASSERT(pStnodType, "expected type spec");

				return pStnodArg;

			} break;
		case '(':	// ( Expression )
			{
				TokNext(pLex); // consume '('

				CSTNode * pStnodReturn = PStnodParseExpression(pParctx, pLex);
				FExpect(pParctx, pLex, TOK(')'));
				return pStnodReturn;
			}

		default: return nullptr;
	}
}

STNode * PStnodParsePostfixExpression(ParseContext * pParctx, Lexer * pLex)
{
	STNode * pStnod = PStnodParsePrimaryExpression(pParctx, pLex);
	if (!pStnod)
		return nullptr;

	while (1)
	{
		if (FIsEndOfStatement(pLex))
			return pStnod;

		switch(pLex->m_tok)
		{
		case TOK('['):		// [ expression ]
			{
				LexSpan lexsp(pLex);
				TokNext(pLex); // consume '('

				STNode * pStnodArray = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
				pStnodArray->m_tok = TOK(pLex->m_tok);
				pStnodArray->m_park = PARK_ArrayElement;
				pStnodArray->IAppendChild(pStnod);

				STNode * pStnodElement = PStnodParseExpression(pParctx, pLex);
				pStnodArray->IAppendChild(pStnodElement);

				pStnod = pStnodArray;
				FExpect(pParctx, pLex, TOK(']'));
			} break;
		case TOK('('):		// ( )
			{				// ( ArgumentExpressionList )
				LexerLocation lexloc(pLex);
				TokNext(pLex); // consume '('

				STNode * pStnodIdent = nullptr;
				if (pStnod->m_park == PARK_Identifier)
				{
					// clear out the identifier's type info
					pStnod->m_pTin = nullptr;
					pStnodIdent = pStnod;
				}

				STNode * pStnodArgList = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
				pStnodArgList->m_tok = TOK(pLex->m_tok);
				pStnodArgList->m_park = PARK_ProcedureCall;
				pStnodArgList->IAppendChild(pStnod);
				pStnod = pStnodArgList;

				// parsing this with LogicalAndOrExpression even though ISO c uses assignmentExpression
				//  need to change this if we expect assignments to return the assigned value (x := a = b; )

				ParseArgumentList(pParctx, pLex, pStnod);

				FExpect(
					pParctx,
					pLex,
					TOK(')'),
					"while parsing procedure call '%s'", 
					pStnodIdent ? StrFromIdentifier(pStnodIdent).PCoz() : "unknown");
			} break;
		case TOK_Arrow:
			{ 
				EmitError(pParctx->m_pWork->m_pErrman, LexSpan(pLex), ERRID_UnknownError, 
					"c-style member dereference '->' is not required, use '.'");

			} // fallthrough
		case TOK('.'):		// . identifier
			{
				TokNext(pLex); // consume '.'
				LexerLocation lexloc(pLex);

				TOK tokPrev = TOK(pLex->m_tok);	
				STNode * pStnodIdent = StnodParseIdentifier(pParctx, pLex);
				if (!pStnodIdent)
				{
					ParseError(pParctx, pLex, "Expected identifier after '.' before %s", PCozFromTok(tokPrev));
				}
				else
				{
					CSTNode * pStnodMember = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
					pStnodMember->m_tok = tokPrev;
					pStnodMember->m_park = PARK_MemberLookup;
					pStnodMember->IAppendChild(pStnod);
					pStnodMember->IAppendChild(pStnodIdent);
					pStnod = pStnodMember;
				}
			} break;
		case TOK_PlusPlus:
		case TOK_MinusMinus:
			{
				LexSpan lexsp(pLex);

				TOK tokPrev = TOK(pLex->m_tok);	
				TokNext(pLex); // consume '++' or '--'

				CSTNode * pStnodUnary = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
				pStnodUnary->m_tok = tokPrev;
				pStnodUnary->m_park = PARK_PostfixUnaryOp;
				pStnodUnary->IAppendChild(pStnod);

				pStnod = pStnodUnary;
			} break;
		default: return pStnod;
		}
	}
}

STNode * PStnodParseUnaryExpression(ParseContext * pParctx, Lexer * pLex)
{
	if (pLex->m_tok == TOK_DoubleReference)
	{
		SplitToken(pLex, TOK_Reference);
	}

	switch(pLex->m_tok)
	{
	case TOK_Identifier:
		{
			if (pLex->m_istr == RWord::g_pChzSizeof || 
				pLex->m_istr == RWord::g_pChzAlignof || 
				pLex->m_istr == RWord::g_pChzTypeinfo || 
				pLex->m_istr == RWord::g_pChzTypeof)
			{
				TOK tokPrev = TOK(pLex->m_tok);	
				auto pStvalRword = PStvalParseReservedWord(pParctx, pLex);

				bool fIsOk = FExpect(pParctx, pLex, TOK('('));

				STNode * pStnodChild = PStnodParseUnaryExpression(pParctx, pLex);
				if (!pStnodChild)
				{
					EmitError(pParctx, LexSpan(pLex), ERRID_MissingOperand, "%s missing argument.", pLex->m_istr.m_pChz);
					fIsOk = false;
				}
				
				fIsOk &= FExpect(pParctx, pLex, TOK(')'));
					
				if (pLex->m_istr == RWord::g_pChzTypeof)
				{
					EmitError(pParctx, LexSpan(pLex), ERRID_FeatureNotImplemented, "typeof not implemented yet.");
					fIsOk = false;
				}

				pStvalRword->m_grfstnod.AssignFlags(FSTNOD_HasParseError, !fIsOk);
				return pStvalRword;
			}
		} break;
	case TOK_Dereference:
	case TOK_Reference:
	case TOK('+'):
	case TOK('-'):
	case TOK('~'):
	case TOK('!'):
	case TOK_PlusPlus:
	case TOK_MinusMinus:
		{
			TOK tokPrev = TOK(pLex->m_tok);	
			LexSpan lexsp(pLex);

			TokNext(pLex); // consume unary operator

			STNode * pStnodExp = PStnodParseUnaryExpression(pParctx, pLex);
			if (!pStnodExp)
			{
				EmitError(
					pParctx,
					LexSpan(pLex),
					ERRID_MissingOperand,
					"Unary operator '%s' missing operand before %s",
					PChzFromTok(tokPrev),
					PChzCurrentToken(pLex));
				return nullptr;
			}

			STNode * pStnodUnary = PSTNodeAlloc<STNode>(pParctx->m_pAlloc, PARK_UnaryOp, pLex, lexsp);
			//CSTNode * pStnodUnary = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
			//pStnodUnary->m_park = PARK_UnaryOp;
			pStnodUnary->m_tok = tokPrev;
			pStnodUnary->SetChildArray(pParctx->m_pAlloc, pStnodExp);

			return pStnodUnary;
		}
	default: break;
	}

	return PStnodParsePostfixExpression(pParctx, pLex);
}

STNode * PStnodParseCastExpression(ParseContext * pParctx, Lexer * pLex)
{
	STNode * pStnodCast = nullptr;
	if (pLex->m_istr != RWord::g_pChzCast)
	{
		return PStnodParseUnaryExpression(pParctx, pLex);
	}

	TokNext(pLex);

	pStnodCast * pStdecl = PSTNodeAlloc<STDecl>(pParctx->m_pAlloc, PARK_Cast, pLex, LexSpan(pLex))
	//auto pStdecl = pStnodCast->PStmapEnsure<CSTDecl>(pParctx->m_pAlloc);

	if (rword == RWORD_Cast)
	{
		FExpect(pParctx, pLex, TOK('('));

		//auto pStnodType = PStnodParseTypeSpecifier(pParctx, pLex, "cast", FPDECL_None);
		pStdecl->m_pStnodType = PStnodParseTypeSpecifier(pParctx, pLex, "cast", FPDECL_None);
		//pStdecl->m_iStnodType = pStnodCast->IAppendChild(pStnodType);

		FExpect(pParctx, pLex, TOK(')'));
	}

	pStdecl->m_pStnodInit = PStnodParseCastExpression(pParctx, pLex);
	if (!pStnodCast)
		return pStnodChild;

	//pStdecl->m_pStnodInit = pStnodCast->IAppendChild(pStnodChild);

	if (pStdecl->m_pStnodInit == nullptr)
	{
		EmitError(pParctx, pStnodCast->m_lexsp, ERRID_UnknownError, "Cast statement missing right hand side");
	}
	return pStnodCast;
}

STNode * PStnodHandleExpressionRHS(
	ParseContext * pParctx,
	Lexer * pLex,
	const LexSpan & lexsp,
	TOK tokExpression,
	PARK parkExpression,
	STNode * pStnodLhs,
	STNode * pStnodRhs)
{
	if (!pStnodRhs)
	{
		EmitError(
			pParctx,
			lexsp,
			ERRID_MissingRhs,
			"operator '%s' missing right hand side before %s",
			PChzFromTok(tokExpression),
			PChzCurrentToken(pLex));
		return pStnodLhs;
	}

	STNode * pStnodExp = PSTNodeAlloc<STOperator>(pParctx->m_pAlloc, parkExpression, pLex, LexSpan(pLex));
	//STNode * pStnodExp = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
	pStnodExp->m_tok = tokExpression;

	STNode * apStnod[] = {pStnodLhs, pStnodRhs};
	pStnodExp->SetChildArray(pParctx->m_pAlloc, apStnod, MOE_DIM(apStnod));
	return pStnodExp;
}

STNode * PStnodParseShiftExpression(ParseContext * pParctx, Lexer * pLex)
{
	STNode * pStnod = PStnodParseCastExpression(pParctx, pLex);
	if (!pStnod)
		return nullptr;

	while (1)
	{
		switch (pLex->m_tok)
		{
		case TOK_ShiftLeft:
		case TOK_ShiftRight:
			{
				LexSpan lexsp(pLex);
				TOK tokPrev = TOK(pLex->m_tok);	
				TokNext(pLex); // consume operator

				STNode * pStnodExp = PStnodParseCastExpression(pParctx, pLex);
				pStnod = PStnodHandleExpressionRHS(pParctx, pLex, lexsp, tokPrev, PARK_ShiftOp, pStnod, pStnodExp);
			} break;
		default: return pStnod;
		}
	}
}

STNode * PStnodParseMultiplicativeExpression(ParseContext * pParctx, Lexer * pLex)
{
	STNode * pStnod = PStnodParseShiftExpression(pParctx, pLex);
	if (!pStnod)
		return nullptr;

	while (1)
	{
		switch (pLex->m_tok)
		{
		case TOK('*'):
		case TOK('/'):
		case TOK('%'):
		case TOK('&'):
			{
				LexSpan lexsp(pLex);
				TOK tokPrev = TOK(pLex->m_tok);	
				TokNext(pLex); // consume operator

				STNode * pStnodExp = PStnodParseShiftExpression(pParctx, pLex);
				pStnod = PStnodHandleExpressionRHS(pParctx, pLex, lexsp, tokPrev, PARK_MultiplicativeOp, pStnod, pStnodExp);
			} break;
		default: return pStnod;
		}
	}
}

STNode * PStnodParseAdditiveExpression(ParseContext * pParctx, Lexer * pLex)
{
	STNode * pStnod = PStnodParseMultiplicativeExpression(pParctx, pLex);
	if (!pStnod)
		return nullptr;

	while (1)
	{
		switch (pLex->m_tok)
		{
		case TOK('+'):
		case TOK('-'):
		case TOK('|'):
		case TOK('^'):
			{
				LexSpan lexsp(pLex);
				TOK tokPrev = TOK(pLex->m_tok);	
				TokNext(pLex); // consume operator

				STNode * pStnodExp = PStnodParseMultiplicativeExpression(pParctx, pLex);
				pStnod = PStnodHandleExpressionRHS(pParctx, pLex, lexsp, tokPrev, PARK_AdditiveOp, pStnod, pStnodExp);
			} break;
		default: return pStnod;
		}
	}
}

STNode * PStnodParseRelationalExpression(ParseContext * pParctx, Lexer * pLex)
{
	STNode * pStnod = PStnodParseAdditiveExpression(pParctx, pLex);
	if (!pStnod)
		return nullptr;

	while (1)
	{
		switch (pLex->m_tok)
		{
		case TOK('<'):
		case TOK('>'):
		case TOK_LessEqual:
		case TOK_GreaterEqual:
		case TOK_EqualEqual:
		case TOK_NotEqual:
			{
				LexSpan lexsp(pLex);
				TOK tokPrev = TOK(pLex->m_tok);	
				TokNext(pLex); // consume operator

				STNode * pStnodExp = PStnodParseAdditiveExpression(pParctx, pLex);
				pStnod = PStnodHandleExpressionRHS(pParctx, pLex, lexsp, tokPrev, PARK_RelationalOp, pStnod, pStnodExp);
			} break;
		default: return pStnod;
		}
	}
}

STNode * PStnodParseLogicalAndExpression(ParseContext * pParctx, Lexer * pLex)
{
	STNode * pStnod = PStnodParseRelationalExpression(pParctx, pLex);
	if (!pStnod)
		return nullptr;

	while (1)
	{
		if (pLex->m_tok == TOK_AndAnd)
		{
			LexSpan lexsp(pLex);
			TOK tokPrev = TOK(pLex->m_tok);
			TokNext(pLex); // consume operator

			STNode * pStnodExp = PStnodParseRelationalExpression(pParctx, pLex);
			pStnod = PStnodHandleExpressionRHS(pParctx, pLex, lexsp, tokPrev, PARK_LogicalAndOrOp, pStnod, pStnodExp);
		}
		else
		{
			return pStnod;
		}
	}
}

STNode * PStnodParseLogicalOrExpression(ParseContext * pParctx, Lexer * pLex)
{
	STNode * pStnod = PStnodParseLogicalAndExpression(pParctx, pLex);
	if (!pStnod)
		return nullptr;

	while (1)
	{
		if (pLex->m_tok == TOK_OrOr)
		{
			LexSpan lexsp(pLex);
			TOK tokPrev = TOK(pLex->m_tok);	
			TokNext(pLex); // consume operator

			STNode * pStnodExp = PStnodParseLogicalAndExpression(pParctx, pLex);
			pStnod = PStnodHandleExpressionRHS(pParctx, pLex, lexsp, tokPrev, PARK_LogicalAndOrOp, pStnod, pStnodExp);
		}
		else
		{
			return pStnod;
		}
	}
}
STNode * PStnodParseAssignmentExpression(ParseContext * pParctx, Lexer * pLex)
{
	STNode * pStnod = PStnodParseLogicalOrExpression(pParctx, pLex);
	if (!pStnod)
		return nullptr;

	while (1)
	{
		switch (pLex->m_tok)
		{
		case TOK('='):
		case TOK_MulEqual:
		case TOK_DivEqual:
		case TOK_ModEqual:
		case TOK_PlusEqual:
		case TOK_MinusEqual:
		case TOK_AndEqual:
		case TOK_OrEqual:
		case TOK_XorEqual:
			{
				LexSpan lexsp(pLex);
				TOK tokPrev = TOK(pLex->m_tok);	
				TokNext(pLex); // consume operator

				STNode * pStnodExp = PStnodParseLogicalOrExpression(pParctx, pLex);
				pStnod = PStnodHandleExpressionRHS(pParctx, pLex, lexsp, tokPrev, PARK_AssignmentOp, pStnod, pStnodExp);
			} break;
		default: return pStnod;
		}
	}
}

STNode * PStnodParseExpression(ParseContext * pParctx, Lexer * pLex, GRFEXP grfexp)
{
	Lexer lexStart = *pLex;

	STNode * pStvalLabel = nullptr;

	if (pLex->m_tok == TOK_Label && grfexp.FIsSet(FEXP_AllowLiteralMemberLabel))
	{
		TokNext(pLex);

		//pStnodLabel = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
		auto pStvalLabel = PSTNodeAlloc<STValue>(pParctx->m_pAlloc, PARK_ArgumentLabel, pLex, LexSpan(pLex));
		pStvalLabel->m_tok = TOK_Label;

		auto pStvalIdent = PStvalParseIdentifier(pParctx, pLex);
		if (!pStvalIdent)
		{
			EmitError(pParctx, LexSpan(pLex), ERRID_MissingLabel, "Argument label did not specify an argument name");
		}
		else
		{
			pStvalLabel->SetChildArray(pParctx->m_pAlloc, pStvalIdent);
		}

		pStvalLabel->AssertValid();
	}

	STNode * pStnodExp = PStnodParseAssignmentExpression(pParctx, pLex);
	if (!pStnodExp)
	{
		*pLex = lexStart;
		if (pStvalLabel)
		{
			STNode * pStnodIdent = pStvalLabel->PStnodChildSafe(0);
			EmitError(pParctx, LexSpan(pLex), ERRID_MissingLabel, "Labeled expression '%s' does not specify a value", StrFromIdentifier(pStnodIdent).PCoz());

			pParctx->m_pAlloc->MOE_DELETE(pStvalLabel);
		}
		return nullptr;
	}

	if (pStnodLabel)
	{
		pStnodLabel->IAppendChild(pStnodExp);
		pStnodExp = pStnodLabel;
	}

	// TODO: handle Expression > AssignmentExpression , AssignmentExpression

	return pStnodExp;
}


STNode * PStnodParseJumpStatement(ParseContext * pParctx, Lexer * pLex)
{
	if (pLex->m_istr == RWord::g_pChzContinue ||
		pLex->m_istr == RWord::g_pChzBreak ||
		pLex->m_istr == RWord::g_pChzFallthrough)
	{
		STNode * pStnod = PStvalParseReservedWord(pParctx, pLex);
		if (pLex->m_tok == TOK_Identifier)
		{
			STValue * pStlit = PSTNodeAlloc<STValue>(pParctx->m_pAlloc, PARK_Identifier, pLex, LexSpan(pLex));
			pStlit->m_istr = pLex->m_istr;
			pStnod->SetChildArray(pParctx->m_pAlloc, pStlit);
			TokNext(pLex);
		}

		ExpectEndOfStatement(pParctx, pLex);
		return pStnod;
	}
	else if (pLex->m_istr == RWord::g_pChzReturn)
	{
		STNode * pStnodReturn = PStvalParseReservedWord(pParctx, pLex);
		if (MOE_FVERIFY(pStnodReturn, "error parsing return"))
		{
			if (!FIsEndOfStatement(pLex))
			{
				STNode * pStnodExp = PStnodParseExpression(pParctx, pLex);
				pStnodReturn->SetChildArray(pParctx->m_pAlloc, pStnodExp);
			}
		}

		ExpectEndOfStatement(pParctx, pLex);
		return pStnodReturn;
	}

	return nullptr;
}

STNode * PStnodParseStatement(ParseContext * pParctx, Lexer * pLex)
{
	STNode * pStnod = PStnodParseCompoundStatement(pParctx, pLex, nullptr);
	if (pStnod)
		return pStnod;

	// Note - Declarations and definition checks need to come first because they peek ahead to see 
	//  if an identifier has ::, : or :=

	pStnod = PStnodParseDecl(pParctx, pLex);
	if (pStnod)
		return pStnod;

#ifdef MOEB_LATER 
	pStnod = PStnodParseDefinition(pParctx, pLex);
	if (pStnod)
		return pStnod;

	pStnod = PStnodParseExpressionStatement(pParctx, pLex);
	if (pStnod)
		return pStnod;

	// handle label for switches or loops

	STIdentifier * pStidentLabel = nullptr;

	if (FConsumeToken(pLex, TOK_Label))
	{
		if (pLex->m_tok != TOK_Identifier)
		{
			EmitError(pParctx, LexSpan(pLex), ERRID_MissingLabel, "Encountered Label directive without label string");
		}
		else
		{
			pStidentLabel = MOE_NEW(pParctx->m_pAlloc, CSTIdentifier) CSTIdentifier();
			pStidentLabel->m_str = pLex->m_str;
			TokNext(pLex);
		}
	}

	pStnod = PStnodParseSelectionStatement(pParctx, pLex, &pStidentLabel);
	
	if (!pStnod)
	{
		pStnod = PStnodParseIterationStatement(pParctx, pLex, &pStidentLabel);
	}

	if (pStidentLabel)
	{
		EmitError(pParctx, pLex, ERRID_FloatingLabel, "Label directive should precede loop or switch statement.");
		pParctx->m_pAlloc->MOE_FREE(pStidentLabel);
	}

	if (pStnod)
		return pStnod;

#endif
	return PStnodParseJumpStatement(pParctx, pLex);
}

bool FParseImportDirectives(ParseContext * pParctx, Lexer * pLex)
{
	if (pLex->m_tok != TOK_Identifier)
		return false;
	
	Workspace * pWork = pParctx->m_pWork;
	Workspace::FILEK filek;
	Moe::InString istrDirective = pLex->m_istr;
	if (istrDirective == RWord::g_pChzImportDirective)
	{
		filek = Workspace::FILEK_Source;
	}
	else if (istrDirective == RWord::g_pChzForeignLibraryDirective)
	{
		filek = Workspace::FILEK_ForeignLibrary;
	}
	else if (istrDirective == RWord::g_pChzStaticLibraryDirective)
	{
		filek = Workspace::FILEK_StaticLibrary;
	}
	else if (istrDirective == RWord::g_pChzDynamicLibraryDirective)
	{
		filek = Workspace::FILEK_DynamicLibrary;
	}
	else 
	{
		return false;
	}

	// BB - Doesn't build an AST node for import directives, won't allow writeback
	TokNext(pLex);
	if (pLex->m_tok == TOK_Literal && pLex->m_litk == LITK_String)
	{
		(void) pWork->PFileEnsure(pLex->m_istr.m_pChz, filek);

		TokNext(pLex);
		return true;
	}
	else
	{
		EmitError(pWork, LexSpan(pLex), ERRID_MissingPath, "expected path following %s directive", istrDirective.m_pChz);
		return false;
	}
}

bool FIsLegalTopLevel(PARK park)
{
	return (park == PARK_Decl) | 
		(park == PARK_ConstantDecl) |
		(park == PARK_Typedef) |
		(park == PARK_ProcedureDefinition) | 
		(park == PARK_EnumDefinition) | 
		(park == PARK_StructDefinition);
}

void ParseTopLevel(ParseContext * pParctx, Lexer * pLex, MoeQuery * pMq)
{
	// load the first token
	TokNext(pLex);

	while (pLex->m_tok != TOK_Eof)
	{
		if (FParseImportDirectives(pParctx, pLex))
			continue;

		Workspace * pWork = pParctx->m_pWork;
		STNode * pStnod = PStnodParseStatement(pParctx, pLex);

		if (!pStnod)
		{
			EmitError(pWork, LexSpan(pLex), ERRID_UnexpectedToken, "Unexpected token at global scope '%s'", PChzCurrentToken(pLex));

			static const TOK s_aTok[] = {TOK(';'), TOK('{') };
			SkipToToken(pLex, s_aTok, MOE_DIM(s_aTok), FLEXER_EndOfLine);
			continue;
		}

		pWork->AppendEntry(pStnod, pParctx->m_pSymtab);

#ifdef MOEB_LATER
		if (!grfunt.FIsSet(FUNT_ImplicitProc))
		{
			if (!FIsLegalTopLevel(pStnod->m_park))
			{
				EmitError(
					pParctx,
					pLex,
					"Unexpected statement at global scope '%s'",
					PChzFromPark(pStnod->m_park));
			}
		}
#endif
	}
}


#define MOE_CHECK_LEXER_SPANS 0
// can move this into the standard unit test code - just make a print routine that outputs the characters in the lexspan
#if MOE_CHECK_LEXER_SPANS
struct ParkSpan // tag = parksp
{
	PARK			m_park;
	const char *	m_pChz;
};

void AssertSpans(STNode * pStnod, ParkSpan * apParksp, int cpParskp)
{

};

void CheckSpans()
{

}
#endif

