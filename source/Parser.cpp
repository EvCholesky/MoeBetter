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

enum FARGLIST
{
	FARGLIST_None				= 0x0,
	FARGLIST_AllowGenericValues = 0x1,

	FARGLIST_All				= 0x1,
};

MOE_DEFINE_GRF(GRFARGLIST, FARGLIST, u32);

STNode * PStnodParseExpression(ParseContext * pParctx, Lexer * pLex, GRFEXP grfexp = FEXP_None);
STNode * PStnodParseLogicalOrExpression(ParseContext * pParctx, Lexer * pLex);
STNode * PStnodParseStatement(ParseContext * pParctx, Lexer * pLex);


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
		STEXK_Decl,		// PARK_CompoundLiteral,	// array/struct literal
		STEXK_None,		// PARK_ArgumentLabel,
		STEXK_None,		// PARK_GenericDecl,
		STEXK_None,		// PARK_GenericStructSpec,		// 
		STEXK_None,		// PARK_TypeArgument,			// raw type, specified to a generic instantiation SFoo(:int)
		STEXK_None,		// PARK_BakedValue,
	};

	MOE_CASSERT(MOE_DIM(s_mpParkStexk) == PARK_Max, "missing PARK STEXK");

	if ((park <= PARK_Nil) | (park >= PARK_Max))
		return STEXK_Nil;

	return s_mpParkStexk[park];
}

void PushSymbolTable(ParseContext * pParctx, SymbolTable * pSymtab)
{
	pSymtab->m_pSymtabParent = pParctx->m_pSymtab;
	pParctx->m_pSymtab = pSymtab;
}

SymbolTable * PSymtabPop(ParseContext * pParctx)
{
	SymbolTable * pSymtabPrev = pParctx->m_pSymtab;
	if (MOE_FVERIFY(pSymtabPrev, "Pop symbol table underflow"))
	{
		pParctx->m_pSymtab = pSymtabPrev->m_pSymtabParent;
	}
	return pSymtabPrev;
}

SymbolTable * PSymtabNew(
	Alloc * pAlloc,
	SymbolTable * pSymtabParent,
	const Moe::InString & istrNamespace,
	TypeRegistry * pTyper,
	UniqueNameSet * pUnsetTin)
{
	SymbolTable * pSymtabNew = MOE_NEW(pAlloc, SymbolTable) SymbolTable(istrNamespace, pAlloc, pTyper, pUnsetTin);
	if (pSymtabParent)
	{
		pSymtabParent->AddManagedSymtab(pSymtabNew);
	}

	return pSymtabNew;
}

SymbolTable * PSymtabNew(Alloc * pAlloc, SymbolTable * pSymtabParent, const Moe::InString & istrNamespace)
{
	if (!MOE_FVERIFY(pSymtabParent, "Null parent passed into pSymtabNew, use other overload for root."))
		return nullptr;

	return PSymtabNew(pAlloc, pSymtabParent, istrNamespace, pSymtabParent->m_pTyper, pSymtabParent->m_pUnset);
}
#if 0
// partial instantiation STEX creation
template <typename T> struct StexAlloc	
{ 
	static T * PStnodAlloc(Moe::Alloc * pAlloc, PARK park, const LexSpan & lexsp)
	{
		return MOE_NEW(pAlloc, T) T(STEXK_None, park, lexsp);
	}
};

template <> struct StexAlloc<STValue>
{ 
	static STValue * PStnodAlloc(Moe::Alloc * pAlloc, PARK park, const LexSpan & lexsp)
	{
		return MOE_NEW(pAlloc, STValue) STValue(park, lexsp);
	}
};

template <> struct StexAlloc<STDecl>
{ 
	static STDecl * PStnodAlloc(Moe::Alloc * pAlloc, PARK park, const LexSpan & lexsp)
	{
		auto pStdecl = MOE_NEW(pAlloc, STDecl) STDecl(park, lexsp);
		pStdecl->SetChildArray(&pStdecl->m_pStnodIdentifier, 3);
		return pStdecl;
	}
};

template <> struct StexAlloc<STProc>
{ 
	static STProc * PStnodAlloc(Moe::Alloc * pAlloc, PARK park, const LexSpan & lexsp)
	{
		auto pStproc = MOE_NEW(pAlloc, STProc) STProc(park, lexsp);
		pStproc->SetChildArray(&pStproc->m_pStnodName, 4);
		return pStproc;
	}
};

template <> struct StexAlloc<STStruct>
{ 
	static STStruct * PStnodAlloc(Moe::Alloc * pAlloc, PARK park, const LexSpan & lexsp)
	{
		auto pStstruct = MOE_NEW(pAlloc, STStruct) STStruct(park, lexsp);
		pStstruct->SetChildArray(&pStstruct->m_pStnodIdentifier, 4);
		return pStstruct;
	}
};

template <> struct StexAlloc<STOperator>
{ 
	static STOperator * PStnodAlloc(Moe::Alloc * pAlloc, PARK park, const LexSpan & lexsp)
	{
		auto pStop = MOE_NEW(pAlloc, STOperator) STOperator(park, lexsp);
		pStop->SetChildArray(&pStop->m_pStnodLhs, 2);
		return pStop;
	}
};
#else

template <typename T> struct StexAlloc	
{ 
	static T * PStnodAlloc(Moe::Alloc * pAlloc, PARK park, const LexSpan & lexsp)
	{
		T * pStnod = MOE_NEW(pAlloc, T) T(park, lexsp);
		pStnod->SetDefaultChildArray();
		return pStnod;
	}
};

template <> struct StexAlloc<STNode>
{ 
	static STNode * PStnodAlloc(Moe::Alloc * pAlloc, PARK park, const LexSpan & lexsp)
	{
		return MOE_NEW(pAlloc, STNode) STNode(STEXK_None, park, lexsp);
	}
};
#endif

template <typename T>
T * PStnodAlloc(Moe::Alloc * pAlloc, PARK park, Lexer * pLex, const LexSpan & lexsp)
{
	T * pStnod = StexAlloc<T>::PStnodAlloc(pAlloc, park, lexsp);

	pStnod->m_tok = TOK(pLex->m_tok);
	return pStnod;
}


STNode * STNode::PStnodChildSafe(int ipStnod)
{
	if (ipStnod < 0 || ipStnod >= m_cpStnodChild)
		return nullptr;

	return m_apStnodChild[ipStnod];
}

void STNode::SetChildArray(STNode ** apStnodChild, size_t cpStnodChild)
{
	m_apStnodChild = (STNode **)apStnodChild;
	m_cpStnodChild = cpStnodChild;
}

void STNode::CopyChildArray(Moe::Alloc * pAlloc, STNode ** apStnodChild, size_t cpStnodChild)
{
	size_t cB = sizeof(STNode *) * cpStnodChild;
	m_apStnodChild = (STNode **)pAlloc->MOE_ALLOC(cB, MOE_ALIGN_OF(STNode *));
	m_cpStnodChild = cpStnodChild;
	memcpy(m_apStnodChild, apStnodChild, cB);
}

void STNode::CopyChildArray(Moe::Alloc * pAlloc, STNode * pStnodChild)
{
	m_cpStnodChild = 1;
	size_t cB = sizeof(STNode *) * m_cpStnodChild;
	m_apStnodChild = (STNode **)pAlloc->MOE_ALLOC(cB, MOE_ALIGN_OF(STNode *));
	memcpy(m_apStnodChild, &pStnodChild, cB);
}

static inline bool FIsValidStnod(STNode * pStnod)
{
	return pStnod && pStnod->m_park != PARK_Error;
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

	STValue * pStval = PStnodAlloc<STValue>(pParctx->m_pAlloc, PARK_Identifier, pLex, LexSpan(pLex));
	pStval->SetIstr(pLex->m_istr);
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

	STValue * pStval = PStnodAlloc<STValue>(pParctx->m_pAlloc, PARK_ReservedWord, pLex, LexSpan(pLex));
	pStval->SetIstr(pLex->m_istr);
	pStval->AssertValid();
	pStval->m_tok = TOK(pLex->m_tok);

	TokNext(pLex);
	return pStval;
}

Moe::InString IstrFromIdentifier(STNode * pStnod)
{
	if (pStnod->m_park == PARK_Identifier)
	{
		STValue * pStval = PStnodRtiCast<STValue*>(pStnod);
		if (pStval)
			return pStval->m_istr;
	}

	return Moe::InString();
}

STNode * PStnodParseExpressionList(
	ParseContext * pParctx,
	Lexer * pLex,
	GRFEXP grfexp)
{
	LexSpan lexsp(pLex);

	STNode * pStnodExp = PStnodParseExpression(pParctx, pLex, grfexp);
	STNode * pStnodList = nullptr;

	if (pStnodExp)
	{
		pStnodList = PStnodAlloc<STNode>(pParctx->m_pAlloc, PARK_ExpressionList, pLex, lexsp);

		CDynAry<STNode *> arypStnod(pParctx->m_pAlloc, BK_Parse);
		arypStnod.Append(pStnodExp);

		while (FConsumeToken(pLex, TOK(',')))
		{
			pStnodExp = PStnodParseExpression(pParctx, pLex, grfexp);

			if (!pStnodExp)
			{
				EmitError(pParctx, LexSpan(pLex), ERRID_ExpectedExpression, "Expected expression before %s", PChzCurrentToken(pLex));
				break;
			}
			arypStnod.Append(pStnodExp);
		}

		pStnodList->SetChildArray(arypStnod.A(), arypStnod.C());
	}

	return pStnodList;
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
				STValue * pStval;
				if (pLex->m_istr == RWord::g_pChzTrue)
				{
					pStval = PStvalParseReservedWord(pParctx, pLex);
					//pStval->SetU64(true);
				}
				else if (pLex->m_istr == RWord::g_pChzFalse)
				{
					pStval = PStvalParseReservedWord(pParctx, pLex);
					//pStval->SetU64(false);
				}
				else if (pLex->m_istr == RWord::g_pChzNull)
				{
					pStval = PStvalParseReservedWord(pParctx, pLex);
					//pStval->SetU64(u32(0));
				}
				else if (pLex->m_istr == RWord::g_pChzFileDirective)
				{
					pStval = PStvalParseReservedWord(pParctx, pLex);

					auto pStvalChild = PStnodAlloc<STValue>(pParctx->m_pAlloc, PARK_Literal, pLex, pStval->m_lexsp);
					pStvalChild->SetIstr(pStval->m_lexsp.m_istrFilename);
				}
				else if (pLex->m_istr == RWord::g_pChzLineDirective)
				{
					pStval = PStvalParseReservedWord(pParctx, pLex);

					LexLookup lexlook(pParctx->m_pWork, pStval);

					auto pStvalChild = PStnodAlloc<STValue>(pParctx->m_pAlloc, PARK_Literal, pLex, pStval->m_lexsp);
					pStvalChild->SetS64(lexlook.m_iLine);
				}
				else
				{
					pStval = PStvalParseIdentifier(pParctx, pLex);

					if (FConsumeToken(pLex, TOK_ColonColon))
					{
#if 1
						MOE_ASSERT(false, "Generic typespec shorthand is WIP");
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
				}
				return pStval;
			}
			/*
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
			}*/
		case TOK_Literal:
			{
				// NOTE - Negative literals don't exist until the type checking phase folds in the unary '-' operator

				auto pStval = PStnodAlloc<STValue>(pParctx->m_pAlloc, PARK_Literal, pLex, LexSpan(pLex));

				if (pLex->m_litk == LITK_Float)
				{
					pStval->SetF64(pLex->m_g);
				}
				else if (pLex->m_litk == LITK_String)
				{
					pStval->SetIstr(pLex->m_istr);
				}
				else if (pLex->m_litk == LITK_Char)
				{
					pStval->SetS64(pLex->m_n);
				}
				else
				{
					pStval->SetU64(pLex->m_n);
				}

				TokNext(pLex);
				return pStval;
			} 
		case TOK(':'): // struct literal
		case TOK('{'): // array literals
			{
				LexSpan lexsp(pLex);
				STNode * pStnodType = nullptr;

				if (pLex->m_tok == TOK(':'))
				{
					// parse type specifier
					TokNext(pLex); // consume ':'
					
#ifdef MOEB_LATER
					pStnodType = PStnodParseTypeSpecifier(pParctx, pLex, "compound literal", FPDECL_AllowBakedTypes | FPDECL_AllowBakedValues);
#endif

					if (!FIsValidStnod(pStnodType))
					{
						EmitError(pParctx, lexsp, ERRID_TypeSpecParseFail, "expected type specification following ':'");
						return pStnodType;
					}
				}

				if (FConsumeToken(pLex, TOK('{')))
				{
					// We're using a decl here... may need a custom structure
					auto pStdeclLit = PStnodAlloc<STDecl>(pParctx->m_pAlloc, PARK_CompoundLiteral, pLex, lexsp);

					pStdeclLit->m_pStnodType = pStnodType;

					STNode * pStnodValues = PStnodParseExpressionList(pParctx, pLex, FEXP_AllowLiteralMemberLabel);
					pStdeclLit->m_pStnodInit = pStnodValues;

					FExpect(pParctx, pLex, TOK('}'), "while parsing struct/array literal");
					return pStdeclLit;
				}

				auto pStnodArg = PStnodAlloc<STNode>(pParctx->m_pAlloc, PARK_TypeArgument, pLex, lexsp);
				pStnodArg->m_tok = TOK(':');
				pStnodArg->CopyChildArray(pParctx->m_pAlloc, pStnodType);

				// MOEB PORT - whats this?
				//auto pTinType = pParctx->m_pSymtab->PTinBuiltin(CSymbolTable::s_strType);
				//pStnodArg->m_pTin = pTinType;
				MOE_ASSERT(pStnodType, "expected type spec");

				return pStnodArg;

			} break;
		case '(':	// ( Expression )
			{
				TokNext(pLex); // consume '('

				STNode * pStnodReturn = PStnodParseExpression(pParctx, pLex);
				FExpect(pParctx, pLex, TOK(')'));
				return pStnodReturn;
			}

		default: return nullptr;
	}
}

//void ParseArgumentList(ParseContext * pParctx, Lexer * pLex, STNode * pStnodArgList, GRFARGLIST grfarglist = FARGLIST_None)
void ParseArgumentList(ParseContext * pParctx, Lexer * pLex, CDynAry<STNode *> * parypStnodArgList, GRFARGLIST grfarglist = FARGLIST_None)
{
	while (1)
	{
		STNode * pStnodLabel = nullptr;
		const char * pChzLabel = "error";
		STNode * pStnodBaked = nullptr;
#ifdef MOEB_LATER
		if (grfarglist.FIsSet(FARGLIST_AllowGenericValues))
		{
			pStnodBaked = PStnodParseBakedConstant(pParctx, pLex, PARK_Decl);
		}
#endif

		if (pStnodBaked)
		{
			parypStnodArgList->Append(pStnodBaked);
			//pStnodArgList->IAppendChild(pStnodBaked);
		}
		else
		{
			CFixAry<STNode *, 2> arypStnodLabel;
			if (pLex->m_tok == TOK_Label)
			{
				TokNext(pLex);

				pStnodLabel = PStnodAlloc<STNode>(pParctx->m_pAlloc, PARK_ArgumentLabel, pLex, LexSpan(pLex));
				pStnodLabel->m_tok = TOK_Label;

				STNode * pStnodIdent = PStvalParseIdentifier(pParctx, pLex);
				if (!pStnodIdent)
				{
					EmitError(pParctx, LexSpan(pLex), ERRID_MissingLabel, "Argument label did not specify an argument name");
				}
				else
				{
					pChzLabel = IstrFromIdentifier(pStnodIdent).m_pChz;
					arypStnodLabel.Append(pStnodIdent);
					//pStnodLabel->CopyChildArray(pParctx->m_pAlloc, pStnodIdent);
				}

				if (FConsumeToken(pLex, TOK('=')))
				{
					EmitError(pParctx, LexSpan(pLex), ERRID_UnexpectedToken, "Labeled arguments do not require an assignment operator");
				}
			}

			STNode * pStnodArg = PStnodParseLogicalOrExpression(pParctx, pLex);
			if (pStnodLabel)
			{
				if (!pStnodArg)
				{
					STNode * pStnodIdent = pStnodLabel->PStnodChildSafe(0);
					EmitError(pParctx, LexSpan(pLex), ERRID_ExpectedExpression, 
						"Labeled argument '%s' does not specify a value", IstrFromIdentifier(pStnodIdent).m_pChz);
				}
				else
				{
					arypStnodLabel.Append(pStnodArg);
					pStnodLabel->CopyChildArray(pParctx->m_pAlloc, arypStnodLabel.A(), arypStnodLabel.C());
					//pStnodLabel->IAppendChild(pStnodArg);
					pStnodArg = pStnodLabel;
				}
			}

			//pStnodArgList->IAppendChild(pStnodArg);
			parypStnodArgList->Append(pStnodArg);

			if ((pStnodArg == nullptr))
				break;
		}

		if (!FConsumeToken(pLex, TOK(',')))
			break;
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

				// BB - Need to push a 'bail out' context... in case of error walk to next ']'
				TokNext(pLex); // consume '['

				auto pStnodArray = PStnodAlloc<STNode>(pParctx->m_pAlloc, PARK_ArrayElement, pLex, lexsp);

				STNode * pStnodElement = PStnodParseExpression(pParctx, pLex);

				STNode * apStnod[] = {pStnod, pStnodElement};
				pStnodArray->CopyChildArray(pParctx->m_pAlloc, apStnod, MOE_DIM(apStnod));

				pStnod = pStnodArray;
				FExpect(pParctx, pLex, TOK(']'));
			} break;
		case TOK('('):		// ( )
			{				// ( ArgumentExpressionList )
				LexSpan lexsp(pLex);
				TokNext(pLex); // consume '('

				STNode * pStnodIdent = nullptr;
				if (pStnod->m_park == PARK_Identifier)
				{
					// clear out the identifier's type info
					// MOEB PORT - but why??? 
					pStnod->m_pTin = nullptr;
					pStnodIdent = pStnod;
				}

				auto pStnodCall = PStnodAlloc<STNode>(pParctx->m_pAlloc, PARK_ProcedureCall, pLex, lexsp);
				//STNode * pStnodArgList = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
				//pStnodArgList->m_tok = TOK(pLex->m_tok);
				//pStnodArgList->m_park = PARK_ProcedureCall;
				CDynAry<STNode *> arypStnodArg(pParctx->m_pAlloc, BK_Parse);
				arypStnodArg.Append(pStnod);

				pStnod = pStnodCall;

				// parsing this with LogicalAndOrExpression even though ISO c uses assignmentExpression
				//  need to change this if we expect assignments to return the assigned value (x := a = b; )

				ParseArgumentList(pParctx, pLex, &arypStnodArg);
				pStnod->CopyChildArray(pParctx->m_pAlloc, arypStnodArg.A(), arypStnodArg.C());

				FExpect(
					pParctx,
					pLex,
					TOK(')'),
					"while parsing procedure call '%s'", 
					pStnodIdent ? IstrFromIdentifier(pStnodIdent).m_pChz : "unknown");
			} break;
		case TOK_Arrow:
			{ 
				EmitError(pParctx, LexSpan(pLex), ERRID_CStyle, "c-style member dereference '->' is not required, use '.'");

			} // fallthrough
		case TOK('.'):		// . identifier
			{
				LexSpan lexsp(pLex);

				TokNext(pLex); // consume '.'
				TOK tokPrev = TOK(pLex->m_tok);	

				STNode * pStnodIdent = PStvalParseIdentifier(pParctx, pLex);
				if (!pStnodIdent)
				{
					EmitError(pParctx, lexsp, ERRID_MissingIdentifier, "Expected identifier after '.' before %s", PChzFromTok(tokPrev));
				}
				else
				{
					auto pStop = PStnodAlloc<STOperator>(pParctx->m_pAlloc, PARK_MemberLookup, pLex, lexsp);
					pStop->m_tok = TOK('.');
					pStop->m_pStnodLhs = pStnod;
					pStop->m_pStnodRhs = pStnodIdent;
					pStnod = pStop;
				}
			} break;
		case TOK_PlusPlus:
		case TOK_MinusMinus:
			{
				LexSpan lexsp(pLex);

				TOK tokPrev = TOK(pLex->m_tok);	
				TokNext(pLex); // consume '++' or '--'

				auto pStop = PStnodAlloc<STOperator>(pParctx->m_pAlloc, PARK_MemberLookup, pLex, lexsp);
				pStop->m_pStnodLhs = pStnod;

				pStnod = pStop;
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

			STOperator * pStop = PStnodAlloc<STOperator>(pParctx->m_pAlloc, PARK_UnaryOp, pLex, lexsp);
			//CSTNode * pStnodUnary = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
			//pStnodUnary->m_park = PARK_UnaryOp;
			pStop->m_tok = tokPrev;
			pStop->m_pStnodLhs = pStnodExp;

			return pStop;
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

	auto pStdecl = PStnodAlloc<STDecl>(pParctx->m_pAlloc, PARK_Cast, pLex, LexSpan(pLex));
	//auto pStdecl = pStnodCast->PStmapEnsure<CSTDecl>(pParctx->m_pAlloc);

	FExpect(pParctx, pLex, TOK('('));

#ifdef MOEB_LATER
	//auto pStnodType = PStnodParseTypeSpecifier(pParctx, pLex, "cast", FPDECL_None);
	pStdecl->m_pStnodType = PStnodParseTypeSpecifier(pParctx, pLex, "cast", FPDECL_None);
	//pStdecl->m_iStnodType = pStnodCast->IAppendChild(pStnodType);
#endif

	FExpect(pParctx, pLex, TOK(')'));

	pStdecl->m_pStnodInit = PStnodParseCastExpression(pParctx, pLex);
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

	STNode * pStnodExp = PStnodAlloc<STOperator>(pParctx->m_pAlloc, parkExpression, pLex, LexSpan(pLex));
	//STNode * pStnodExp = EWC_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
	pStnodExp->m_tok = tokExpression;

	STNode * apStnod[] = {pStnodLhs, pStnodRhs};
	pStnodExp->CopyChildArray(pParctx->m_pAlloc, apStnod, MOE_DIM(apStnod));
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
	Lexer pLexStart = *pLex;

	STNode * pStnodLabelIdent = nullptr;
//	CDynAry<STNode *> arypStnodLabel(pParctx->m_pAlloc, BK_Parse);
	if (pLex->m_tok == TOK_Label && grfexp.FIsSet(FEXP_AllowLiteralMemberLabel))
	{
		TokNext(pLex);

		pStnodLabelIdent = PStvalParseIdentifier(pParctx, pLex);
		if (!pStnodLabelIdent)
		{
			EmitError(pParctx, LexSpan(pLex), ERRID_MissingLabel, "Argument label did not specify an argument name");
		}
	}

	STNode * pStnodExp = PStnodParseAssignmentExpression(pParctx, pLex);
	if (!pStnodExp)
	{
		*pLex = pLexStart;
		if (pStnodLabelIdent)
		{
			EmitError(pParctx, LexSpan(pLex), ERRID_ExpectedExpression, "Labeled expression '%s' does not specify a value", IstrFromIdentifier(pStnodLabelIdent).m_pChz);

			pParctx->m_pAlloc->MOE_DELETE(pStnodLabelIdent);
			pStnodLabelIdent = nullptr;
		}
		return nullptr;
	}

	STValue * pStvalLabel = nullptr;
	if (pStnodLabelIdent)
	{
		auto pStvalLabel = PStnodAlloc<STValue>(pParctx->m_pAlloc, PARK_ArgumentLabel, pLex, LexSpan(pLex));
		pStvalLabel->m_tok = TOK_Label;

		STNode * apStnod[] = {pStnodLabelIdent, pStnodExp};
		pStvalLabel->CopyChildArray(pParctx->m_pAlloc, apStnod, MOE_DIM(apStnod));

		pStnodExp = pStvalLabel;
	}

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
			STValue * pStlit = PStnodAlloc<STValue>(pParctx->m_pAlloc, PARK_Identifier, pLex, LexSpan(pLex));
			pStlit->m_istr = pLex->m_istr;
			pStnod->CopyChildArray(pParctx->m_pAlloc, pStlit);
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
				pStnodReturn->CopyChildArray(pParctx->m_pAlloc, pStnodExp);
			}
		}

		ExpectEndOfStatement(pParctx, pLex);
		return pStnodReturn;
	}

	return nullptr;
}

STNode * PStnodParseExpressionStatement(ParseContext * pParctx, Lexer * pLex)
{
	if (FConsumeToken(pLex, TOK(';')))
	{
		// return empty statement

		auto pStnodEmpty = PStnodAlloc<STNode>(pParctx->m_pAlloc, PARK_Nop, pLex, LexSpan(pLex));
		return pStnodEmpty;
	}

	STNode * pStnod = PStnodParseExpression(pParctx, pLex);
	if (pStnod)
	{
		ExpectEndOfStatement(pParctx, pLex);
	}
	return pStnod;
}

STNode * PStnodParseCompoundStatement(ParseContext * pParctx, Lexer * pLex, SymbolTable * pSymtab)
{
	STNode * pStnodList = nullptr;

	if (FConsumeToken(pLex, TOK('{')))
	{
		CDynAry<STNode *> arypStnod(pParctx->m_pAlloc, BK_Parse);
		pStnodList = PStnodAlloc<STNode>(pParctx->m_pAlloc, PARK_List, pLex, LexSpan(pLex));
		if (!pSymtab)
		{
			pSymtab = PSymtabNew(pParctx->m_pAlloc, pParctx->m_pSymtab, "anon");
		}

		pStnodList->m_pSymtab = pSymtab;
		PushSymbolTable(pParctx, pSymtab);

		while (pLex->m_tok != TOK('}'))
		{
			STNode * pStnod = PStnodParseStatement(pParctx, pLex);
			if (!pStnod)
				break;

			if (pStnod->m_grfstnod.FIsAnySet(FSTNOD_EntryPoint))
			{
				// Note - move the lexLoc for this entry to be the lexloc for the block so nested
				//  functions appear at the beginning of the containing scope (yuck!)
				pStnod->m_lexsp = pStnodList->m_lexsp;

				pParctx->m_pWork->AppendEntry(pStnod, pParctx->m_pSymtab);
			}
			else 
			{
				arypStnod.Append(pStnod);
			}
		}

		pStnodList->CopyChildArray(pParctx->m_pAlloc, arypStnod.A(), arypStnod.C());

		SymbolTable * pSymtabPop = PSymtabPop(pParctx);
		MOE_ASSERT(pSymtab == pSymtabPop, "CSymbol table push/pop mismatch (list)");
		FExpect(pParctx, pLex, TOK('}'));
	}

	return pStnodList;
}

STNode * PStnodParseStatement(ParseContext * pParctx, Lexer * pLex)
{
	STNode * pStnod = PStnodParseCompoundStatement(pParctx, pLex, nullptr);
	if (pStnod)
		return pStnod;

	// Note - Declarations and definition checks need to come first because they peek ahead to see 
	//  if an identifier has ::, : or :=

#ifdef MOEB_LATER 
	pStnod = PStnodParseDecl(pParctx, pLex);
	if (pStnod)
		return pStnod;

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

