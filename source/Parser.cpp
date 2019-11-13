#include "Error.h"
#include "Generics.inl"
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

static int g_nSymtabVisitId = 1; // visit index used by symbol table collision walks

const char * PChzFromLitk(LITK litk)
{
	static const char * s_mpLitkPChz[] =
	{
		"Numeric",
		"Char",
		"String",
		"Bool",
		"Null",
		"Enum",
		"Compound",
		"Pointer",
	};
	MOE_CASSERT(MOE_DIM(s_mpLitkPChz) == LITK_Max, "missing LITK string");
	if (litk == LITK_Nil)
		return "Nil";

	if ((litk < LITK_Nil) | (litk >= LITK_Max))
		return "Unknown LITK";

	return s_mpLitkPChz[litk];
}

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

const char * PChzFromQualk(QUALK qualk)
{
	static const char * s_mpQualkPChz[] =
	{
		"const",
		"inarg",
	};
	MOE_CASSERT(MOE_DIM(s_mpQualkPChz) == QUALK_Max, "missing QUALK string");
	if (qualk == QUALK_Nil)
		return "Nil";

	if ((qualk < QUALK_Nil) | (qualk >= QUALK_Max))
		return "Unknown QUALK";

	return s_mpQualkPChz[qualk];
}


void AppendFlagNames(Moe::StringBuffer * pStrbuf, GRFQUALK grfqualk, const char * pChzSpacer)
{
	const char * pChzSpacerCur = "";
	for (int qualk = QUALK_Min; qualk < QUALK_Max; ++qualk)
	{
		if (grfqualk.FIsSet(0x1 << qualk))
		{
			AppendChz(pStrbuf, pChzSpacerCur);
			AppendChz(pStrbuf, PChzFromQualk((QUALK)qualk));
			pChzSpacerCur = pChzSpacer;
		}
	}
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

void PrintTypeInfo(Moe::StringBuffer * pStrbuf, TypeInfo * pTin, PARK park, GRFDBGSTR grfdbgstr = FDBGSTR_None)
{
	if (pTin == nullptr)
	{
		switch (park)
		{
		case PARK_ExpressionList:
		case PARK_List:				AppendChz(pStrbuf, "{}");		return;
		case PARK_Identifier:		AppendChz(pStrbuf, "Ident");	return;
		case PARK_ParameterList:	AppendChz(pStrbuf, "Params");	return; 
		case PARK_VariadicArg:		AppendChz(pStrbuf, "..");		return;
		case PARK_Nop:				AppendChz(pStrbuf, "Nop");		return;
		case PARK_Uninitializer:	AppendChz(pStrbuf, "---");		return;
		case PARK_AssignmentOp:		AppendChz(pStrbuf, "=");		return;
		case PARK_ConstantDecl:		AppendChz(pStrbuf, "constdecl");return;
		case PARK_Decl:				AppendChz(pStrbuf, "decl");		return;
		case PARK_ArrayDecl:		AppendChz(pStrbuf, "arydecl");	return;
		default:					AppendChz(pStrbuf, "unk");		return;
		}
	}

	switch (pTin->m_tink)
	{
	case TINK_Qualifier:
		{
			auto pTinqual = (TypeInfoQualifier *)pTin;
			const char * pChzSpacer = (grfdbgstr.FIsSet(FDBGSTR_NoWhitespace)) ? "." : " ";
			AppendFlagNames(pStrbuf, pTinqual->m_grfqualk, pChzSpacer);
			AppendChz(pStrbuf, pChzSpacer);
			PrintTypeInfo(pStrbuf, pTinqual->m_pTin, park, grfdbgstr);
			return;
		} break;
	case TINK_Pointer:		
		{
			auto pTinptr = (TypeInfoPointer*)pTin;
			AppendChz(pStrbuf, PChzFromTok(TOK_Reference));
			PrintTypeInfo(pStrbuf, pTinptr->m_pTin, park, grfdbgstr);
			return;
		}
	case TINK_Array:
		{
			TypeInfoArray * pTinary = (TypeInfoArray*)pTin;

			switch (pTinary->m_aryk)
			{
			case ARYK_Fixed:		FormatChz(pStrbuf, "[%d]", pTinary->m_c);	break;
			case ARYK_Dynamic:		AppendChz(pStrbuf, "[..]");					break;
			case ARYK_Reference:	AppendChz(pStrbuf, "[]");					break;
			default: 
				MOE_ASSERT(false, "Unhandled ARYK");
				break;
			}

			PrintTypeInfo(pStrbuf, pTinary->m_pTin, park, grfdbgstr);
			return;
		}

	case TINK_Literal:		
		{
			auto pTinlit = (TypeInfoLiteral *)pTin;

			if (pTinlit->m_litty.m_litk == LITK_Enum)
			{
				PrintTypeInfo(pStrbuf, pTinlit->m_pTinSource, PARK_Nil, grfdbgstr);
				AppendChz(pStrbuf, " ");
			}
			else if (!grfdbgstr.FIsSet(FDBGSTR_LiteralSize))
			{
				AppendChz(pStrbuf, PChzFromLitk(pTinlit->m_litty.m_litk));
				AppendChz(pStrbuf, " ");

				if (pTinlit->m_litty.m_litk == LITK_Compound && pTinlit->m_pTinSource)
				{
					AppendChz(pStrbuf, "(");
					PrintTypeInfo(pStrbuf, pTinlit->m_pTinSource, PARK_Nil, grfdbgstr);
					AppendChz(pStrbuf, ")");
				}
			}
			
			AppendChz(pStrbuf, "Literal");
			return;
		}
	case TINK_Anchor:
		{
			auto pTinanc = (TypeInfoAnchor *)pTin;
			FormatChz(pStrbuf, "$%s", pTinanc->m_istrName.m_pChz);
			return;	
		}
    case TINK_Procedure:
		{
			auto pTinproc = (TypeInfoProcedure *)pTin;
			FormatChz(pStrbuf, "%s(", pTin->m_istrName.m_pChz);

			size_t cpTin = pTinproc->m_arypTinParams.C();
			size_t cCommas = (pTinproc->FHasVarArgs()) ? cpTin : cpTin - 1;
			for (size_t ipTin = 0; ipTin < cpTin; ++ipTin)
			{
				PrintTypeInfo(pStrbuf, pTinproc->m_arypTinParams[ipTin], PARK_Nil, grfdbgstr);

				if (ipTin < cCommas)
				{
					AppendChz(pStrbuf, ", ");
				}
			}

			if (pTinproc->FHasVarArgs())
			{
				AppendChz(pStrbuf, "..");
			}

			AppendChz(pStrbuf, ")->");

			cpTin = pTinproc->m_arypTinReturns.C();
			for (size_t ipTin = 0; ipTin < cpTin; ++ipTin)
			{
				PrintTypeInfo(pStrbuf, pTinproc->m_arypTinReturns[ipTin], PARK_Nil, grfdbgstr);
			}

			if (pTinproc->m_grftinproc.FIsSet(FTINPROC_IsForeign))
			{
				AppendChz(pStrbuf, " #foreign");
			}

			return;
		}
    case TINK_Struct:
		{
			FormatChz(pStrbuf, "%s", pTin->m_istrName.m_pChz);

			auto pTinstruct = (TypeInfoStruct *)pTin;
			if (pTinstruct->m_pGenmap)
			{
				PrintGenmapAnchors(pStrbuf, pTinstruct->m_pGenmap);
			}
			else if (pTinstruct->FHasGenericParams() && grfdbgstr.FIsSet(FDBGSTR_ShowStructArgs))
			{
				auto pStnodStruct = pTinstruct->m_pStnodStruct;
				STStruct * pStstruct = nullptr;

				if (pStnodStruct)
				{
					pStstruct = PStnodRtiCast<STStruct *>(pStnodStruct);
				}

				if (MOE_FVERIFY(pStstruct && pStstruct->m_pStnodParameterList, "expected parameter list"))
				{
					auto pStnodParameterList = pStstruct->m_pStnodParameterList;
					size_t cpStnodParam = pStnodParameterList->m_cpStnodChild;
					const char * pChzSeparate = "(";
					const char * pChzClose = ")";
					for (int ipStnodParam = 0; ipStnodParam < cpStnodParam; ++ipStnodParam)
					{
						auto pStnodParam = pStnodParameterList->PStnodChild(ipStnodParam);
						auto pStdecl = PStnodRtiCast<STDecl *>(pStnodParam);
						if (!pStdecl)
							continue;
	
						FormatChz(pStrbuf, "%s", pChzSeparate);
						pChzSeparate = ", ";
						pChzClose = ")";
						if (pStdecl->m_pStnodIdentifier)
						{
							auto istrIdent = IstrFromIdentifier(pStdecl->m_pStnodIdentifier);
							FormatChz(pStrbuf, "$%s ", istrIdent.m_pChz);
						}

						if (pStdecl->m_pStnodType)
						{
							auto istrType = IstrFromTypeInfo(pStdecl->m_pStnodType->m_pTin);
							FormatChz(pStrbuf, ":%s", istrType.m_pChz);
						}
					}
					AppendChz(pStrbuf, pChzClose);
				}
			}

			return;
		}
    case TINK_Enum:
		{
			FormatChz(pStrbuf, "%s_enum", pTin->m_istrName.m_pChz);
			return;
		}
	case TINK_Numeric:
		{
			if (!grfdbgstr.FIsSet(FDBGSTR_UseSizedNumerics))
			{
				AppendChz(pStrbuf, pTin->m_istrName.m_pChz);
				return;
			}

			// print out the size resolved type (rather than any type aliases - ie. int)
			auto pTinn = (TypeInfoNumeric *)pTin;
			char chSigned;
			if (pTinn->m_grfnum.FIsSet(FNUM_IsFloat))
			{
				chSigned = 'f';
			}
			else
			{
				chSigned = (pTinn->m_grfnum.FIsAnySet(FNUM_IsSigned)) ? 's' : 'u';
			}

			switch(pTinn->m_cBit)
			{
			case 8:		FormatChz(pStrbuf, "%c8", chSigned);	break;
			case 16:	FormatChz(pStrbuf, "%c16", chSigned);	break;
			case 32:	FormatChz(pStrbuf, "%c32", chSigned);	break;
			case 64:	FormatChz(pStrbuf, "%c64", chSigned);	break;
			default: MOE_ASSERT(false, "unknown numeric size");
			}

			return;
		}
    case TINK_Bool:			// fall through ...
    case TINK_Flag:			// fall through ...
    case TINK_Void:			// fall through ...
    case TINK_Null:			// fall through ...
    case TINK_Any:			// fall through ...
    case TINK_Type:			// fall through ...
		AppendChz(pStrbuf, pTin->m_istrName.m_pChz);
		break;
	default:
		MOE_ASSERT(false, "unhandled TINK in PrintTypeInfo");
		return;
	}
}


void PrintLiteral(Moe::StringBuffer * pStrbuf, STNode * pStnodLit)
{
	auto pStvalLit = PStnodRtiCast<STValue * >(pStnodLit);
	if (!MOE_FVERIFY(pStnodLit->m_park == PARK_Literal && pStvalLit, "bad literal in PrintLiteral"))
		return;

	switch (pStvalLit->m_stvalk)
	{
	case STVALK_String:			FormatChz(pStrbuf, "\"%s\"", pStvalLit->m_istr.m_pChz);			return;
	case STVALK_UnsignedInt:	FormatChz(pStrbuf, "%llu", pStvalLit->m_nUnsigned);				return;
	case STVALK_SignedInt:		FormatChz(pStrbuf, "%lld", pStvalLit->m_nSigned);				return;
	case STVALK_Float:			FormatChz(pStrbuf, "%f", pStvalLit->m_g);						return;
	case STVALK_ReservedWord:	FormatChz(pStrbuf, "%s", pStvalLit->m_istr.m_pChz);				return;
	default:
		MOE_ASSERT(false, "unknown literal %s", PChzFromTok(pStnodLit->m_tok));
		return;
	}
}

void PrintStnodName(Moe::StringBuffer * pStrbuf, STNode * pStnod)
{
	if (!pStnod)
	{
		AppendChz(pStrbuf, "null");
		return;
	}

	switch (pStnod->m_park)
	{
	case PARK_Identifier:			FormatChz(pStrbuf, "%s", IstrFromIdentifier(pStnod).m_pChz);	return;
	case PARK_ReservedWord:			FormatChz(pStrbuf, "%s", IstrFromIdentifier(pStnod).m_pChz);	return;
	case PARK_Nop:					AppendChz(pStrbuf, "nop");										return;
	case PARK_Literal:				
		{
			PrintLiteral(pStrbuf, pStnod);
			return;
		}
	case PARK_AdditiveOp:		    FormatChz(pStrbuf, "%s", PChzFromTok(pStnod->m_tok));				return;
	case PARK_MultiplicativeOp:	    FormatChz(pStrbuf, "%s", PChzFromTok(pStnod->m_tok));				return;
	case PARK_ShiftOp:			    FormatChz(pStrbuf, "%s", PChzFromTok(pStnod->m_tok));				return;
	case PARK_RelationalOp:		    FormatChz(pStrbuf, "%s", PChzFromTok(pStnod->m_tok));				return;
	case PARK_LogicalAndOrOp:	    FormatChz(pStrbuf, "%s", PChzFromTok(pStnod->m_tok));				return;
	case PARK_UnaryOp:			    FormatChz(pStrbuf, "unary[%s]", PChzFromTok(pStnod->m_tok));		return;
	case PARK_PostfixUnaryOp:		FormatChz(pStrbuf, "postUnary[%s]", PChzFromTok(pStnod->m_tok));	return;
	case PARK_AssignmentOp:		    FormatChz(pStrbuf, "%s", PChzFromTok(pStnod->m_tok));				return;
	case PARK_ArrayElement:		    AppendChz(pStrbuf, "elem");					return;
	case PARK_MemberLookup:		    AppendChz(pStrbuf, "member");				return;
	case PARK_ProcedureCall:		AppendChz(pStrbuf, "procCall");				return;
	case PARK_SpecializedStruct:	AppendChz(pStrbuf, "specStruct");			return;
	case PARK_ExpressionList:
	case PARK_List:				    AppendChz(pStrbuf, "{}");					return;
	case PARK_ParameterList:	    AppendChz(pStrbuf, "params");				return;
	case PARK_If:				    AppendChz(pStrbuf, "if");					return;
	case PARK_Else:				    AppendChz(pStrbuf, "else");					return;
	case PARK_ArrayDecl:		    AppendChz(pStrbuf, "[]");					return;
	case PARK_ProcedureReferenceDecl:
									AppendChz(pStrbuf, "procref");				return;
	case PARK_Uninitializer:		AppendChz(pStrbuf, "---");					return;
	case PARK_ReferenceDecl:		AppendChz(pStrbuf, "ptr");					return;
#ifdef MOEB_LATER
	case PARK_QualifierDecl:		AppendChz(pStrbuf, PChzFromRword(pStnod->m_pStval->m_rword));	return;
#endif
	case PARK_Decl:					AppendChz(pStrbuf, "decl");					return;
	case PARK_Typedef:				AppendChz(pStrbuf, "typedef");				return;
	case PARK_ConstantDecl:			AppendChz(pStrbuf, "const");				return;
	case PARK_ProcedureDefinition:	AppendChz(pStrbuf, "func");					return;
	case PARK_EnumDefinition:		AppendChz(pStrbuf, "enum");					return;
	case PARK_StructDefinition:		AppendChz(pStrbuf, "struct");				return;
	case PARK_EnumConstant:			AppendChz(pStrbuf, "enumConst");			return;
	case PARK_VariadicArg:			AppendChz(pStrbuf, "..");					return;
	case PARK_CompoundLiteral:		AppendChz(pStrbuf, "compLit");				return;
	case PARK_Cast:					AppendChz(pStrbuf, "cast");					return;
	case PARK_ArgumentLabel:		AppendChz(pStrbuf, "`"); 					return;
	case PARK_GenericDecl:			AppendChz(pStrbuf, "gendecl"); 				return;
	case PARK_GenericStructSpec:	AppendChz(pStrbuf, "genstruct");			return;
	case PARK_TypeArgument:			AppendChz(pStrbuf, "typearg");				return;
	case PARK_Error:
	default:						AppendChz(pStrbuf, "error");				return;
	}
}

InString IstrFromTypeInfo(TypeInfo * pTin)
{
	if (!pTin)
	{
		return IstrIntern("null");
	}

	char aCh[1024];
	Moe::StringBuffer strbuf(aCh, MOE_DIM(aCh));

	PrintTypeInfo(&strbuf, pTin, PARK_Nil);
	return IstrInternCopy(aCh);
}

InString IstrFromStnod(STNode * pStnod)
{
	if (!pStnod)
	{
		return IstrIntern("null");
	}

	char aCh[1024];
	Moe::StringBuffer strbuf(aCh, MOE_DIM(aCh));

	PrintStnodName(&strbuf, pStnod);
	return IstrInternCopy(aCh);
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

SymbolTable::SUsing::~SUsing()
{
	auto pAlloc = m_hashHvPSymp.PAlloc();
	CHash<HV, SymbolPath *>::CIterator iterPSymp(&m_hashHvPSymp);
	while (SymbolPath ** ppSymp = iterPSymp.Next())
	{
		pAlloc->MOE_DELETE(*ppSymp);
	}
}

SymbolTable::~SymbolTable()
{
	CHash<Moe::InString, Symbol *>::CIterator iterPSym(&m_hashIstrPSym);
	while (Symbol ** ppSym = iterPSym.Next())
	{
		Symbol * pSym = *ppSym;
		while (pSym)
		{
			Symbol * pSymDelete = pSym;
			pSym = pSym->m_pSymPrev;

			m_pAlloc->MOE_DELETE(pSymDelete);
		}

		*ppSym = nullptr;
	}

	for (Symbol ** ppSym = m_arypSymGenerics.A(); ppSym != m_arypSymGenerics.PMac(); ++ppSym)
	{
		m_pAlloc->MOE_DELETE(*ppSym);
	}

#ifdef MOEB_LATER
	CHash<HV, STypeInfoForwardDecl *>::CIterator iterPTinfwd(&m_hashHvPTinfwd);
	while (TypeInfoForwardDecl ** ppTinfwd = iterPTinfwd.Next())
	{
		MOE_ASSERT((*ppTinfwd)->m_arypTinReferences.C() == 0, "unresolved forward declarations");
	}
#endif

	for (TypeInfo ** ppTin = m_arypTinManaged.A(); ppTin != m_arypTinManaged.PMac(); ++ppTin)
	{
		DeleteTypeInfo(m_pAlloc, *ppTin);
		*ppTin = nullptr;
	}
}

enum SYMCOLLIS 
{
	SYMCOLLIS_SymbolName,
	SYMCOLLIS_CyclicUsing,

	MOE_MAX_MIN_NIL(SYMCOLLIS)
};

enum SYMCCK		// SYMbol Collision Check Kind
{
	SYMCCK_Entry,
	SYMCCK_UsedBy,	// walking symbol tables that use the entry table
	SYMCCK_Uses,	// walking symtabs that the entry table uses
};

SYMCOLLIS SymcollisCheck(
	SymbolTable * pSymtab,
	const Moe::InString * pIstrMin,
	const Moe::InString * pIstrMax,
	u64 nVisitId,
	FSHADOW fshadow,
	STNode ** ppStnodCollision, 
	SYMCCK symcck = SYMCCK_Entry,
	SymbolTable * pSymtabUser = nullptr)
{
	if (pSymtab->m_nVisitId == nVisitId)
	{
		*ppStnodCollision = nullptr;
		return SYMCOLLIS_CyclicUsing;
	}
	pSymtab->m_nVisitId = nVisitId;

	SYMCOLLIS symcollis = SYMCOLLIS_Nil;
	if (fshadow == FSHADOW_NoShadowing)
	{
		for (const InString * pIstr = pIstrMin; pIstr != pIstrMax; ++pIstr)
		{
			Symbol ** ppSym = pSymtab->m_hashIstrPSym.Lookup(*pIstr);
			if (ppSym)
			{
				*ppStnodCollision = (*ppSym)->m_pStnodDefinition;
				symcollis = SYMCOLLIS_SymbolName;
				break;
			}
		}
	}

	// We need to check all of the symbol tables 'derived' from the one that started our recursive check, but not
	//   tables derived from the symtabs we are using. Also we need to make sure we don't recurse back up the symtab path 
	//   that brought us here.
	if (symcck != SYMCCK_Uses )
	{
		auto ppSymtabMax = pSymtab->m_arypSymtabUsedBy.PMac();
		for (auto ppSymtabIt = pSymtab->m_arypSymtabUsedBy.A(); ppSymtabIt != ppSymtabMax; ++ppSymtabIt)
		{
			auto pSymtabUsedBy = *ppSymtabIt;
			symcollis = moeMax(symcollis, SymcollisCheck(pSymtabUsedBy, pIstrMin, pIstrMax, nVisitId, FSHADOW_NoShadowing, ppStnodCollision, SYMCCK_UsedBy, pSymtab));
			if (symcollis == SYMCOLLIS_CyclicUsing)
				return symcollis;
		}
	}

	auto pUsingMax = pSymtab->m_aryUsing.PMac();
	for (auto pUsingIt = pSymtab->m_aryUsing.A(); pUsingIt != pUsingMax; ++pUsingIt)
	{
		if (pUsingIt->m_pSymtab == pSymtabUser)
			continue;

		symcollis = moeMax(symcollis, SymcollisCheck(pUsingIt->m_pSymtab, pIstrMin, pIstrMax, nVisitId, FSHADOW_NoShadowing, ppStnodCollision, SYMCCK_Uses));
		if (symcollis == SYMCOLLIS_CyclicUsing)
			return symcollis;
	}

	return symcollis;
}


ERRID ErridCheckSymbolCollision(
	ErrorManager * pErrman,
	const LexSpan & lexsp,
	const char * pChzContext,
	SymbolTable * pSymtabContext,
	const Moe::InString * pIstrMin,
	const Moe::InString * pIstrMax,
	FSHADOW fshadow,
	u64 nVisitId)
{
	STNode * pStnodCollision = nullptr;
	auto symcollis = SymcollisCheck(pSymtabContext, pIstrMin, pIstrMax, nVisitId, fshadow, &pStnodCollision);
	if (symcollis != SYMCOLLIS_Nil)
	{
		//s32 iLine = 0;
		//s32 iCol = 0;
		//const char * pChzFilename = "unknown";
		LexLookup lexlook;
		const char * pChzSymName = "";
		if (pStnodCollision)
		{
			lexlook = LexLookup(pErrman->m_pWork, pStnodCollision->m_lexsp);
			//CalculateLinePosition(pErrman->m_pWork, &pStnodCollision->m_lexloc, &iLine, &iCol);
			//pChzFilename = pStnodCollision->m_lexloc.m_strFilename.PCoz();

			if (pStnodCollision->m_pSymbase && pStnodCollision->m_pSymbase->m_symk == SYMK_Symbol)
			{
				pChzSymName = ((Symbol *)pStnodCollision->m_pSymbase)->m_istrName.m_pChz;
			}
		}

		switch (symcollis)
		{
		case SYMCOLLIS_SymbolName: EmitError(pErrman, lexsp, ERRID_UsingStatementCollision, 
										"%s shadows symbol name '%s' at %s(%d, %d)", 
										pChzContext,
										pChzSymName,
										lexlook.m_istrFilename.m_pChz, lexlook.m_iLine, lexlook.m_iCodepoint);
									return ERRID_UsingStatementCollision;
		case SYMCOLLIS_CyclicUsing: EmitError(pErrman, lexsp, ERRID_UsingStatementCycle, 
										"%s causes using statement cycle at %s(%d, %d)", 
										pChzContext,
										lexlook.m_istrFilename.m_pChz, lexlook.m_iLine, lexlook.m_iCodepoint);
									return ERRID_UsingStatementCycle;
		}
	}
	return ERRID_Nil;
}

Symbol * SymbolTable::PSymNewUnmanaged(const InString & istrName, STNode * pStnodDefinition, GRFSYM grfsym)
{
	auto pSym = MOE_NEW(m_pAlloc, Symbol) Symbol;
	pSym->m_symk = SYMK_Symbol;
	pSym->m_symdep = SYMDEP_Nil;

	pSym->m_aryPSymReferencedBy.SetAlloc(m_pAlloc, BK_Dependency, 4);
	pSym->m_aryPSymHasRefTo.SetAlloc(m_pAlloc, BK_Dependency, 4);

	pSym->m_istrName = istrName;
	pSym->m_pStnodDefinition = pStnodDefinition;
	pSym->m_grfsym = grfsym;
	//pSym->m_pTin = nullptr;
	pSym->m_pSymPrev = nullptr;
	return pSym;
}

Symbol * SymbolTable::PSymEnsure(
	ErrorManager * pErrman,
	const InString & istrName,
	STNode * pStnodDefinition,
	GRFSYM grfsym,
	FSHADOW fshadow)
{
	LexSpan lexsp;
	if (pStnodDefinition)
		lexsp = pStnodDefinition->m_lexsp;

	(void) ErridCheckSymbolCollision(
		pErrman, 
		lexsp,
		istrName.m_pChz,
		this,
		&istrName, &istrName + 1,
		fshadow,
		++g_nSymtabVisitId);	// BB - not threadsafe

	Symbol * pSymPrev = nullptr;
	Symbol * pSym = nullptr;
	Symbol ** ppSym = m_hashIstrPSym.Lookup(istrName);
	if (ppSym)
	{
		pSym = *ppSym;
		if (pSym->m_pStnodDefinition != pStnodDefinition)
		{
			pSymPrev = pSym;
			pSym = nullptr;

			if (fshadow != FSHADOW_ShadowingAllowed)
			{
				LexLookup lexlook;
				if (pSymPrev->m_pStnodDefinition)
				{
					lexlook = LexLookup(pErrman->m_pWork, pSymPrev->m_pStnodDefinition->m_lexsp);
				}

				EmitError(pErrman, lexsp, ERRID_ShadowedDefine, "%s symbol shadows previous type definition at %s(%d, %d)", 
					istrName.m_pChz,
					lexlook.m_istrFilename.m_pChz,
					lexlook.m_iLine,
					lexlook.m_iCodepoint);
			}
		}
	}
	
	if (!pSym)
	{
		pSym = PSymNewUnmanaged(istrName, pStnodDefinition, grfsym);
		(void) m_hashIstrPSym.InresEnsureKeyAndValue(istrName, pSym);
	}

	pSym->m_pSymPrev = pSymPrev;
	return pSym;
}


TypeInfo * SymbolTable::PTinBuiltin(const Moe::InString & istr)
{
	TypeInfo ** ppTin = m_hashIstrPTinBuiltIn.Lookup(istr);
	if (ppTin)
		return *ppTin;

	return nullptr;

	/* // pre-MOEB we looked these up by symbol->m_pTin 
	if (inres == INRES_Inserted)
	LexSpan lexsp;
	auto pSym = PSymLookup(istr, lexsp);
	if (pSym)
		return pSym->m_pTin;
	return nullptr;
	*/
}

void SymbolTable::AddBuiltInType(ErrorManager * pErrman, Lexer * pLex, TypeInfo * pTin, GRFSYM grfsym)
{
	// NOTE: This function is for built-in types without a lexical order, so we shouldn't be calling it on an ordered table
	MOE_ASSERT(m_iNestingDepth == 0, "Cannot add built-in types to ordered symbol table.");

	m_arypTinManaged.Append(pTin);
	const InString & istrName = pTin->m_istrName;
	if (!MOE_FVERIFY(!istrName.FIsEmpty(), "registering unnamed type"))
		return;

	TypeInfo ** ppTinValue = nullptr;
	INRES inres = m_hashIstrPTinBuiltIn.InresEnsureKey(istrName, &ppTinValue);
	if (inres == INRES_Inserted)
	{
		*ppTinValue = pTin;

		auto pSym = PSymEnsure(pErrman, istrName, nullptr, FSYM_IsBuiltIn | FSYM_IsType | FSYM_VisibleWhenNested | grfsym.m_raw);
		//pSym->m_pTin = pTin;
	}
	else
	{
		LexSpan lexsp = (pLex) ? LexSpan(pLex) : LexSpan();
		EmitError(pErrman, lexsp, ERRID_ShadowedDefine, "Two types encountered with same name (%s)", istrName.m_pChz);
	}
}


void AddSimpleBuiltInType(Workspace * pWork, SymbolTable * pSymtab, const char * pChzName, TINK tink, GRFSYM grfsym = FSYM_None)
{
	InString istrName = IstrIntern(pChzName);
	TypeInfo * pTin = MOE_NEW(pSymtab->m_pAlloc, TypeInfo) TypeInfo(
																istrName,
																tink);

	pSymtab->AddBuiltInType(pWork->m_pErrman, nullptr, pTin, grfsym);
}

void AddBuiltInNumeric(Workspace * pWork, SymbolTable * pSymtab, const char * pChzName, u32 cBit, GRFNUM grfnum)
{
	InString istrName = IstrIntern(pChzName);
	TypeInfoNumeric * pTinn = MOE_NEW(pSymtab->m_pAlloc, TypeInfoNumeric) TypeInfoNumeric(
																				istrName,
																				cBit,
																				grfnum);
	pSymtab->AddBuiltInType(pWork->m_pErrman, nullptr, pTinn);
}

void AddBuiltInAlias(Workspace * pWork, SymbolTable * pSymtab, const char * pChzNameNew, const char * pChzNameOld)
{
	InString istrNameNew = IstrIntern(pChzNameNew);
	InString istrNameOld = IstrIntern(pChzNameOld);
	TypeInfo *	pTinOld = pSymtab->PTinBuiltin(istrNameOld);

	if (MOE_FVERIFY(pTinOld, "bad built in alias") &&
		MOE_FVERIFY(pTinOld->m_tink == TINK_Numeric, "unsupported built-in alias type"))
	{
		auto pTinnOld = PTinDerivedCast<TypeInfoNumeric *>(pTinOld);
		auto pTinnNew = MOE_NEW(pSymtab->m_pAlloc, TypeInfoNumeric) 
							TypeInfoNumeric( istrNameNew, pTinnOld->m_cBit, pTinnOld->m_grfnum);

		pTinnNew->m_pTinUnaliased = (pTinOld->m_pTinUnaliased) ? pTinOld->m_pTinUnaliased : pTinOld;
		pSymtab->AddBuiltInType(pWork->m_pErrman, nullptr, pTinnNew);
	}
}

void AddBuiltInLiteral(Workspace * pWork, SymbolTable * pSymtab, const char * pChzName, LITK litk, s8 cBit, GRFNUM grfnum)
{
	InString istrName = IstrIntern(pChzName);
	TypeInfoLiteral * pTinlit = MOE_NEW(pSymtab->m_pAlloc, TypeInfoLiteral) TypeInfoLiteral();
	pTinlit->m_istrName = istrName;
	pSymtab->AddBuiltInType(nullptr, nullptr, pTinlit);

	pTinlit->m_litty.m_litk = litk;
	pTinlit->m_litty.m_cBit = cBit;
	pTinlit->m_litty.m_grfnum = grfnum;
	pTinlit->m_fIsFinalized = true;

	Moe::CDynAry<TypeInfoLiteral *> * paryPTinlit = &pSymtab->m_mpLitkArypTinlit[litk];
	if (!paryPTinlit->m_pAlloc)
	{
		paryPTinlit->SetAlloc(pSymtab->m_pAlloc, Moe::BK_Parse, 8);
	}
	paryPTinlit->Append(pTinlit);
}


void SymbolTable::AddBuiltInSymbols(Workspace * pWork)
{
	AddSimpleBuiltInType(pWork, this, BuiltIn::g_pChzBool, TINK_Bool);
	AddSimpleBuiltInType(pWork, this, "_flag", TINK_Flag, FSYM_InternalUseOnly);
	AddSimpleBuiltInType(pWork, this, "void", TINK_Void);
	AddSimpleBuiltInType(pWork, this, BuiltIn::g_pChzString, TINK_Type);

	AddBuiltInNumeric(pWork, this, BuiltIn::g_pChzU8, 8, FNUM_None);
	AddBuiltInNumeric(pWork, this, BuiltIn::g_pChzU16, 16, FNUM_None);
	AddBuiltInNumeric(pWork, this, BuiltIn::g_pChzU32, 32, FNUM_None);
	//AddBuiltInNumeric(pWork, this, "char", 32, FNUM_None);
	AddBuiltInNumeric(pWork, this, BuiltIn::g_pChzU64, 64, FNUM_None);

	AddBuiltInNumeric(pWork, this, BuiltIn::g_pChzS8, 8, FNUM_IsSigned);
	AddBuiltInNumeric(pWork, this, BuiltIn::g_pChzS16, 16, FNUM_IsSigned);
	AddBuiltInNumeric(pWork, this, BuiltIn::g_pChzS32, 32, FNUM_IsSigned);
	AddBuiltInNumeric(pWork, this, BuiltIn::g_pChzS64, 64, FNUM_IsSigned);

	// BB - This is wrong, it should be based on a runtime parameter for the TARGET's word size, not the compiler's word size
#if MOE_X64
	AddBuiltInAlias(pWork, this, BuiltIn::g_pChzInt, "s64");
	AddBuiltInAlias(pWork, this, BuiltIn::g_pChzUint, "u64");
	AddBuiltInAlias(pWork, this, BuiltIn::g_pChzSSize, "s64");
	AddBuiltInAlias(pWork, this, BuiltIn::g_pChzUSize, "u64");
#else
	AddBuiltInAlias(pWork, this, BuiltIn::g_pChzInt, "s32");
	AddBuiltInAlias(pWork, this, BuiltIn::g_pChzUint, "u32");
	AddBuiltInAlias(pWork, this, BuiltIn::g_pChzSsize, "s32");
	AddBuiltInAlias(pWork, this, BuiltIn::g_pChzUsize, "u32");
#endif

	AddBuiltInNumeric(pWork, this, BuiltIn::g_pChzF32, 32, FNUM_IsSigned | FNUM_IsFloat);
	AddBuiltInNumeric(pWork, this, BuiltIn::g_pChzF64, 64, FNUM_IsSigned | FNUM_IsFloat);
	AddBuiltInAlias(pWork, this, BuiltIn::g_pChzFloat, "f32");
	//AddBuiltInAlias(pWork, this, BuiltIn::g_pChzDouble, "f64");

	AddBuiltInLiteral(pWork, this, "__bool_Literal", LITK_Bool, 8, FNUM_None);
	AddBuiltInLiteral(pWork, this, "__u8_Literal", LITK_Numeric, 8, FNUM_None);
	AddBuiltInLiteral(pWork, this, "__u16_Literal", LITK_Numeric, 16, FNUM_None);
	AddBuiltInLiteral(pWork, this, "__u32_Literal", LITK_Numeric, 32, FNUM_None);
	AddBuiltInLiteral(pWork, this, "__u64_Literal", LITK_Numeric, 64, FNUM_None);
	AddBuiltInLiteral(pWork, this, "__s8_Literal", LITK_Numeric, 8, FNUM_IsSigned);
	AddBuiltInLiteral(pWork, this, "__s16_Literal", LITK_Numeric, 16, FNUM_IsSigned);
	AddBuiltInLiteral(pWork, this, "__s32_Literal", LITK_Numeric, 32, FNUM_IsSigned);
	AddBuiltInLiteral(pWork, this, "__s64_Literal", LITK_Numeric, 64, FNUM_IsSigned);
	AddBuiltInLiteral(pWork, this, "__f32_Literal", LITK_Numeric, 32, FNUM_IsSigned | FNUM_IsFloat);
	AddBuiltInLiteral(pWork, this, "__f64_Literal", LITK_Numeric, 64, FNUM_IsSigned | FNUM_IsFloat);
	AddBuiltInLiteral(pWork, this, "__string_Literal", LITK_String, -1, FNUM_IsSigned);
	AddBuiltInLiteral(pWork, this, "__char_Literal", LITK_Char, 32, FNUM_IsSigned);
	AddBuiltInLiteral(pWork, this, "__void_Literal", LITK_Null, -1, FNUM_None);
}

void DeleteTypeInfo(Alloc * pAlloc, TypeInfo * pTin)
{
	pAlloc->MOE_DELETE(pTin);
}

void SymbolTable::AddManagedSymtab(SymbolTable * pSymtab)
{
	MOE_ASSERT(pSymtab->m_pSymtabNextManaged == nullptr, "trying to add managed symtab");

	pSymtab->m_pSymtabNextManaged = m_pSymtabNextManaged;
	m_pSymtabNextManaged = pSymtab;
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

void STNode::AssertValid()
{
	STEXK stexk = StexkFromPark(m_park);

	switch (stexk)
	{
	case STEXK_None:												break;
	case STEXK_For:			((STFor*)this)->AssertValid();			break;
	case STEXK_Decl:		((STDecl*)this)->AssertValid();			break;
	case STEXK_Enum:		((STEnum*)this)->AssertValid();			break;
	case STEXK_Struct:		((STStruct*)this)->AssertValid();		break;
	case STEXK_Proc:		((STProc*)this)->AssertValid();			break;
	case STEXK_Value:		((STValue*)this)->AssertValid();		break;
	case STEXK_Operator:	((STOperator*)this)->AssertValid();		break;
	default:
		MOE_ASSERT(false, "missing assert valid for STNode type");
		break;
	}
}

void STFor::AssertValid()
{
	MOE_ASSERT(StexkFromPark(m_park) == STEXK_For, "bad park mapping");
}

void STDecl::AssertValid()
{
	MOE_ASSERT(StexkFromPark(m_park) == STEXK_Decl, "bad park mapping");
}

void STEnum::AssertValid()
{
	MOE_ASSERT(StexkFromPark(m_park) == STEXK_Enum, "bad park mapping");
}

void STStruct::AssertValid()
{
	MOE_ASSERT(StexkFromPark(m_park) == STEXK_Struct, "bad park mapping");
}

void STProc::AssertValid()
{
	MOE_ASSERT(StexkFromPark(m_park) == STEXK_Proc, "bad park mapping");
}

void STValue::AssertValid()
{
	MOE_ASSERT(StexkFromPark(m_park) == STEXK_Value, "bad park mapping");
}

void STOperator::AssertValid()
{
	MOE_ASSERT(StexkFromPark(m_park) == STEXK_Operator, "bad park mapping");
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
						CSTNode * pStnodSpec = MOE_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);

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
				CSTNode * pStnod = MOE_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);

				pStnod->m_tok = TOK(pLex->m_tok);
				pStnod->m_park = PARK_Literal;

				CSTValue * pStval = MOE_NEW(pParctx->m_pAlloc, CSTValue) CSTValue();
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

				if (pLex->m_litk == LITK_String)
				{
					pStval->SetIstr(pLex->m_istr);
				}
				else if (pLex->m_litk == LITK_Char)
				{
					pStval->SetS64(pLex->m_n);
				}
				else
				{
					MOE_ASSERT(pLex->m_litk == LITK_Numeric, "unknown literal kind");

					if (pLex->m_grfnum.FIsSet(FNUM_IsFloat))
					{
						pStval->SetF64(pLex->m_g);
					}
					else
					{
						pStval->SetU64(pLex->m_n);
					}
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
				//STNode * pStnodArgList = MOE_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
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
			//CSTNode * pStnodUnary = MOE_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
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
	//STNode * pStnodExp = MOE_NEW(pParctx->m_pAlloc, CSTNode) CSTNode(pParctx->m_pAlloc, lexloc);
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
			pSymtab = PSymtabNew(pParctx->m_pAlloc, pParctx->m_pSymtab, IstrIntern("anon"));
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

