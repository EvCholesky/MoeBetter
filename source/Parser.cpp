#include "Error.h"
#include "Generics.inl"
#include "Parser.h"
#include "Request.h"
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

void PushSymbolTable(ParseContext * pParctx, SymbolTable * pSymtab);
SymbolTable * PSymtabPop(ParseContext * pParctx);

void ParseArgumentList(ParseContext * pParctx, Lexer * pLex, CDynAry<STNode *> * parypStnodArgList, GRFARGLIST grfarglist = FARGLIST_None);
STNode * PStnodParseDefinition(ParseContext * pParctx, Lexer * pLex);
STNode * PStnodParseExpression(ParseContext * pParctx, Lexer * pLex, GRFEXP grfexp = FEXP_None);
STNode * PStnodParseLogicalOrExpression(ParseContext * pParctx, Lexer * pLex);
STNode * PStnodParseStatement(ParseContext * pParctx, Lexer * pLex);
STValue * PStvalParseIdentifier(ParseContext * pParctx, Lexer * pLex);
STValue * PStvalAllocateIdentifier(ParseContext * pParctx, Lexer * pLex, const LexSpan & lexsp, const Moe::InString istrIdent);

static int g_nSymtabVisitId = 1; // visit index used by symbol table collision walks

struct ProcSymtabStack // tag = procss
{
						ProcSymtabStack(ParseContext * pParctx)
						:m_pParctx(pParctx)
						,m_pSymtabPrev(nullptr)
							{ ; }

						~ProcSymtabStack()
						{
							if (m_pSymtabPrev)
								PSymtabPop();
						}

	void				Push(SymbolTable * pSymtab, const LexSpan & lexloc)
							{
								m_pSymtabPrev = m_pParctx->m_pSymtabGeneric;
								m_pParctx->m_pSymtabGeneric = pSymtab;

								::PushSymbolTable(m_pParctx, pSymtab);
							}

	SymbolTable * 		PSymtabPop()
							{
								m_pParctx->m_pSymtabGeneric = m_pSymtabPrev;
								return ::PSymtabPop(m_pParctx);
							}

	ParseContext *		m_pParctx; 
	SymbolTable *		m_pSymtabPrev;
};


struct ParkInfo // tag = parkinfo
{
	STEXK			m_stexk;
	const char *	m_pChzAbbrev;
	const char *	m_pChzLong;
};

static ParkInfo s_mpParkParkinfo[] =
{
	{ STEXK_Node,		"err", "Error" },
	{ STEXK_Value,		"ident", "Identifier" },
	{ STEXK_Value,		"rword", "Reserved Word" },
	{ STEXK_Node,		"nop", "Nop" },
	{ STEXK_Value,		"lit",	"Literal" },
	{ STEXK_Operator,	"addOp", "Additive Operator" },
	{ STEXK_Operator,	"mulOp", "Multiplicative Operator" },
	{ STEXK_Operator,	"shiftOp", "Shift Operator" },
	{ STEXK_Operator,	"relOp", "Relational Operator" },
	{ STEXK_Operator,	"logicOp", "LogicalAndOr Operator" },
	{ STEXK_Operator,	"assignOp", "Assignment Operator" },
	{ STEXK_Operator,	"prefixOp", "Unary Operator" },
	{ STEXK_Operator,	"postfixOp", "Postfix Unary Operator" },
	{ STEXK_Node,		"uninit", "Uninitializer" },
	{ STEXK_Node,		"cast", "Cast" },
	{ STEXK_Node,		"elem", "Array Element" },			// [array, index]
	{ STEXK_Operator,	"memb", "Member Lookup" },			// [struct, child]
	{ STEXK_Node,		"call", "Procedure Call"},			// [procedure, arg0, arg1, ...]
	{ STEXK_Node,		"specStruct", "Specialized Struct" },
	{ STEXK_Node,		"list", "List", },
	{ STEXK_Node,		"params", "Parameter List" },
	{ STEXK_Node,		"expList", "Expression List" },
	{ STEXK_Node,		"genType", "Generic Type Spec" },
	{ STEXK_Node,		"if" ,"If" },
	{ STEXK_Node,		"else", "Else" },
	{ STEXK_For,		"for", "For" },
	{ STEXK_Node,		"aryDecl", "Array Decl" },
	{ STEXK_Node,		"refDecl", "Reference Decl" },
	{ STEXK_Node,		"qualDecl", "Qualifier Decl" },
	{ STEXK_Node,		"procDecl", "Procedure Reference Decl" },
	{ STEXK_Decl,		"decl", "Decl" },
	{ STEXK_Decl,		"constDecl", "Constant Decl" },
	{ STEXK_Node,		"typedef", "Typedef" },
	{ STEXK_Proc,		"procDef", "Procedure Definition" },
	{ STEXK_Enum,		"enumDef", "Enum Definition" },
	{ STEXK_Struct,		"strucDef", "Struct Definition" },
	{ STEXK_Decl,		"enumConst", "Enum Constant" },
	{ STEXK_Node,		"varArg", "Variadic Argument" },
	{ STEXK_Decl,		"cpdLit", "CompoundLiteral" },
	//{ STEXK_None,		"aryLit", "Array Literal" },
	{ STEXK_Node,		"argLabel", "Argument Label" },
	{ STEXK_Node,		"genDecl", "Generic Decl" },
	{ STEXK_Node,		"genStruct", "Generic Struct Spec" },
	{ STEXK_Node,		"typeArg", "Type Argument" },
	{ STEXK_Node,		"baked", "Baked Value" },
};

MOE_CASSERT(MOE_DIM(s_mpParkParkinfo) == PARK_Max, "missing ParkInfo");


ErrorManager * ParseContext::PErrman() const
{
	return m_pWork->m_pErrman; 
}

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

bool FNeedsImplicitMember(ENUMIMP enumimp, ENUMK enumk)
{
	if (enumimp == ENUMIMP_Names || enumimp == ENUMIMP_Values)
		return true;
	bool fIsFlagEnumimp = enumimp > ENUMIMP_MaxConstant;
	return fIsFlagEnumimp == (enumk == ENUMK_FlagEnum);
}

Moe::InString IstrFromEnumimp(ENUMIMP enumimp)
{
	const InString mpEnumimpIstr[] =
	{
	BuiltIn::g_istrEnumNil,		// ENUMIMP_NilConstant
	BuiltIn::g_istrEnumMin,		// ENUMIMP_MinConstant
	BuiltIn::g_istrEnumLast,	// ENUMIMP_LastConstant
	BuiltIn::g_istrEnumMax,		// ENUMIMP_MaxConstant
	BuiltIn::g_istrEnumNone,	// ENUMIMP_None
	BuiltIn::g_istrEnumAll,		// ENUMIMP_All
	BuiltIn::g_istrEnumNames,	// ENUMIMP_Names
	BuiltIn::g_istrEnumValues,	// ENUMIMP_Values
	};

	if (enumimp == ENUMIMP_Nil)
		return IstrIntern("Nil");

	if ((enumimp < ENUMIMP_Nil) | (enumimp >= ENUMIMP_Max))
		return IstrIntern("Unknown EnumImp");

	return mpEnumimpIstr[enumimp];
}

const char * PChzAbbrevFromPark(PARK park)
{
	if (park == PARK_Nil)
		return "Nil";

	if ((park < PARK_Nil) | (park >= PARK_Max))
		return "Unknown PARK";

	return s_mpParkParkinfo[park].m_pChzAbbrev;
}

const char * PChzLongFromPark(PARK park)
{
	if (park == PARK_Nil)
		return "Nil";

	if ((park < PARK_Nil) | (park >= PARK_Max))
		return "Unknown PARK";

	return s_mpParkParkinfo[park].m_pChzLong;
}

STEXK StexkFromPark(PARK park)
{
	if ((park <= PARK_Nil) | (park >= PARK_Max))
		return STEXK_Nil;

	return s_mpParkParkinfo[park].m_stexk;
}

const char * PChzFromStexk(STEXK stexk)
{
	static const char * s_mpStexkPChz[] =
	{
	"Node",
	"For",
	"Decl",
	"Enum",
	"Struct",
	"Proc",
	"Value",
	"Operator",
	};
	MOE_CASSERT(MOE_DIM(s_mpStexkPChz) == STEXK_Max, "missing STEXK string");
	if (stexk == STEXK_Nil)
		return "Nil";

	if ((stexk < STEXK_Nil) | (stexk >= STEXK_Max))
		return "Unknown STEXK";

	return s_mpStexkPChz[stexk];
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

inline bool FIsIdentifier(STNode * pStnod, InString istr)
{
	auto pStval = PStnodRtiCast<STValue *>(pStnod);

	if (pStval->m_park != PARK_Identifier)
		return false;

	return pStval->m_istr == istr;
}


void WriteTypeInfoSExpression(Moe::StringBuffer * pStrbuf, TypeInfo * pTin, PARK park, GRFSEW grfsew)
{
	AppendChz(pStrbuf, ":");

	if (pTin == nullptr)
	{
		if (park != PARK_Nil)
		{
			FormatChz(pStrbuf, "null(%s)", PChzAbbrevFromPark(park));
		}
		else
		{
			AppendChz(pStrbuf, "null");
		}
		return;
	}

	switch (pTin->m_tink)
	{
	case TINK_Qualifier:
		{
			auto pTinqual = (TypeInfoQualifier *)pTin;
			const char * pChzSpacer = (grfsew.FIsSet(FSEW_NoWhitespace)) ? "." : " ";
			AppendFlagNames(pStrbuf, pTinqual->m_grfqualk, pChzSpacer);
			AppendChz(pStrbuf, pChzSpacer);
			WriteTypeInfoSExpression(pStrbuf, pTinqual->m_pTin, park, grfsew);
			return;
		} break;
	case TINK_Pointer:		
		{
			auto pTinptr = (TypeInfoPointer*)pTin;
			AppendChz(pStrbuf, PChzFromTok(TOK_Reference));
			WriteTypeInfoSExpression(pStrbuf, pTinptr->m_pTin, park, grfsew);
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

			WriteTypeInfoSExpression(pStrbuf, pTinary->m_pTin, park, grfsew);
			return;
		}

	case TINK_Literal:		
		{
			auto pTinlit = (TypeInfoLiteral *)pTin;

			if (pTinlit->m_litty.m_litk == LITK_Enum)
			{
				WriteTypeInfoSExpression(pStrbuf, pTinlit->m_pTinSource, PARK_Nil, grfsew);
				AppendChz(pStrbuf, " ");
			}
			else if (!grfsew.FIsSet(FSEW_LiteralSize))
			{
				AppendChz(pStrbuf, PChzFromLitk(pTinlit->m_litty.m_litk));
				AppendChz(pStrbuf, " ");

				if (pTinlit->m_litty.m_litk == LITK_Compound && pTinlit->m_pTinSource)
				{
					AppendChz(pStrbuf, "(");
					WriteTypeInfoSExpression(pStrbuf, pTinlit->m_pTinSource, PARK_Nil, grfsew);
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
				WriteTypeInfoSExpression(pStrbuf, pTinproc->m_arypTinParams[ipTin], PARK_Nil, grfsew);

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
				WriteTypeInfoSExpression(pStrbuf, pTinproc->m_arypTinReturns[ipTin], PARK_Nil, grfsew);
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
			else if (pTinstruct->FHasGenericParams() && grfsew.FIsSet(FSEW_ShowStructArgs))
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
							WriteTypeInfoSExpression(pStrbuf, pStdecl->m_pStnodType->m_pTin, pStdecl->m_pStnodType->m_park);
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
			if (!grfsew.FIsSet(FSEW_UseSizedNumerics))
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

/*
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
	default:
		MOE_ASSERT(false, "unknown literal %s", PChzFromTok(pStnodLit->m_tok));
		return;
	}
}

void WriteAstValue(Moe::StringBuffer * pStrbuf, STNode * pStnod)
{
	auto pStval = PStnodRtiCast<STValue *>(pStnod);
	if (!pStval)
	{
		AppendChz(pStrbuf, "novalue");
		return;
	}

	auto  pTinlit = PTinRtiCast<TypeInfoLiteral *>(pStnod->m_pTin);
	auto stvalk = pStval->m_stvalk;
	if (pTinlit)
	{
		switch (pTinlit->m_litty.m_litk)
		{
			case LITK_Char:		// fallthrough
			case LITK_Enum:		// fallthrough
			case LITK_Numeric:	
			{
				if (pTinlit->m_litty.m_grfnum.FIsSet(FNUM_IsFloat))
				{
					stvalk = STVALK_Float;
				}
				else
				{
					stvalk = (pTinlit->m_litty.m_grfnum.FIsSet(FNUM_IsSigned)) ? STVALK_SignedInt : STVALK_UnsignedInt;
				}
			} break;
			case LITK_String:	stvalk = STVALK_String;																break;
			case LITK_Bool:		FormatChz(pStrbuf, "%s", (pStval->m_nUnsigned == 0) ? "false" : "true");			return;
			case LITK_Null:		AppendChz(pStrbuf, "null");															return;
			case LITK_Compound:
				{
#ifdef MOEB_LATER
					AppendChz(pStrbuf, "(");

					auto pStdecl = PStmapRtiCast<CSTDecl *>(pStnod->m_pStmap);
					if (!MOE_FVERIFY(pStdecl && pStdecl->m_iStnodInit >= 0, "array literal with no values"))
						break;

					auto pStnodList = pStnod->PStnodChild(pStdecl->m_iStnodInit);
					for (int ipStnod = 0; ipStnod < pStnodList->CStnodChild(); ++ipStnod)
					{
						PrintStval(pStrbuf, pStnodList->PStnodChild(ipStnod));
						if (ipStnod + 1 < pStnodList->CStnodChild())
						{
							AppendChz(pStrbuf, ", ");
						}
					}

					AppendChz(pStrbuf, ")");
#endif
				} break;
		}
	}

	switch (stvalk)
	{
	case STVALK_Float:			FormatChz(pStrbuf, "%f", pStval->m_g);												return;
	case STVALK_SignedInt:		FormatChz(pStrbuf, "%lld", pStval->m_nSigned);										return;
	case STVALK_UnsignedInt:	FormatChz(pStrbuf, "%llu", pStval->m_nUnsigned);									return;
	case STVALK_String:			FormatChz(pStrbuf, "'%s'", pStval->m_istr.m_pChz);									return;
	}
}
*/

bool FTryWriteValueSExpression(Moe::StringBuffer * pStrbuf, STNode * pStnod)
{
	auto pStval = PStnodRtiCast<STValue *>(pStnod);
	if (!pStval)
		return false;

	auto  pTinlit = PTinRtiCast<TypeInfoLiteral *>(pStnod->m_pTin);
	auto stvalk = pStval->m_stvalk;
	if (pTinlit)
	{
		switch (pTinlit->m_litty.m_litk)
		{
			case LITK_Char:		// fallthrough
			case LITK_Enum:		// fallthrough
			case LITK_Numeric:	
			{
				if (pTinlit->m_litty.m_grfnum.FIsSet(FNUM_IsFloat))
				{
					stvalk = STVALK_Float;
				}
				else
				{
					stvalk = (pTinlit->m_litty.m_grfnum.FIsSet(FNUM_IsSigned)) ? STVALK_SignedInt : STVALK_UnsignedInt;
				}
			} break;
			case LITK_String:	
				stvalk = STVALK_String;																
				break;

			case LITK_Bool:		
				FormatChz(pStrbuf, "%s", (pStval->m_nUnsigned == 0) ? "false" : "true");			
				return true;

			case LITK_Null:		
				AppendChz(pStrbuf, "null");															
				return true;

			case LITK_Compound:
				{
#ifdef MOEB_LATER
					AppendChz(pStrbuf, "(");

					auto pStdecl = PStmapRtiCast<CSTDecl *>(pStnod->m_pStmap);
					if (!MOE_FVERIFY(pStdecl && pStdecl->m_iStnodInit >= 0, "array literal with no values"))
						break;

					auto pStnodList = pStnod->PStnodChild(pStdecl->m_iStnodInit);
					for (int ipStnod = 0; ipStnod < pStnodList->CStnodChild(); ++ipStnod)
					{
						PrintStval(pStrbuf, pStnodList->PStnodChild(ipStnod));
						if (ipStnod + 1 < pStnodList->CStnodChild())
						{
							AppendChz(pStrbuf, ", ");
						}
					}

					AppendChz(pStrbuf, ")");
					break;
#else
					return false;
#endif
				} 
		}
	}

	switch (stvalk)
	{
	case STVALK_Float:			FormatChz(pStrbuf, "%f", pStval->m_g);								break;
	case STVALK_SignedInt:		FormatChz(pStrbuf, "%lld", pStval->m_nSigned);						break;
	case STVALK_UnsignedInt:	FormatChz(pStrbuf, "%llu", pStval->m_nUnsigned);					break;
	case STVALK_String:			FormatChz(pStrbuf, "'%s'", pStval->m_istr.m_pChz);					break;
	default: 
		return false;
	}

	return true;
}

void WriteParseKindSExpression(Moe::StringBuffer * pStrbuf, STNode * pStnod, GRFSEW grfsew)
{
	if (!pStnod)
	{
		AppendChz(pStrbuf, "null");
		return;
	}

	FormatChz(pStrbuf, "%s", PChzAbbrevFromPark(pStnod->m_park));
}

void WriteParseSExpression(Moe::StringBuffer * pStrbuf, STNode * pStnod, GRFSEW grfsew)
{
	if (!pStnod)
	{
		AppendChz(pStrbuf, "null");
		return;
	}

	if (FTryWriteValueSExpression(pStrbuf, pStnod))
		return;

	if (pStnod->Stexk() == STEXK_Operator)
	{
		FormatChz(pStrbuf, "op%s", PChzFromTok(pStnod->m_tok));
		return;
	}

	if (pStnod->m_pTin)
	{
		WriteTypeInfoSExpression(pStrbuf, pStnod->m_pTin, pStnod->m_park, grfsew);
		return;
	}

	WriteParseKindSExpression(pStrbuf, pStnod, grfsew);
}

/*
void WriteAstName(Moe::StringBuffer * pStrbuf, STNode * pStnod)
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
*/

void WriteStnodFromSewk(StringBuffer * pStrbuf, STNode * pStnod, SEWK sewk, GRFSEW grfsew)
{
	switch(sewk)
	{
	case SEWK_Park:
		{
			WriteParseKindSExpression(pStrbuf, pStnod, grfsew);
		} break;
	case SEWK_Parse:
		{
			WriteParseSExpression(pStrbuf, pStnod, grfsew);
		} break;
	case SEWK_Value:
		{
			if(!FTryWriteValueSExpression(pStrbuf, pStnod))
			{
				AppendChz(pStrbuf, "_");
			}
		} break;
	case SEWK_TypeInfo:	
		{
			if (pStnod->m_pTin == nullptr && (pStnod->m_park == PARK_Identifier || pStnod->m_park == PARK_ReservedWord))
			{
				(void)FTryWriteValueSExpression(pStrbuf, pStnod);
			}
			else
			{
				WriteTypeInfoSExpression(pStrbuf, pStnod->m_pTin, pStnod->m_park, grfsew);
			}
		} break;
	default:
		MOE_ASSERT(false, "unhandled SExpression Kind");
	}

	if (grfsew.FIsSet(FSEW_LiteralSize))
	{
		if (pStnod->m_pTin && pStnod->m_pTin->m_tink == TINK_Literal)
		{
			const LiteralType & litty = ((TypeInfoLiteral *)pStnod->m_pTin)->m_litty;

			FormatChz(pStrbuf, ":%s", PChzFromLitk(litty.m_litk));

			if (litty.m_cBit >= 0)
			{
				FormatChz(pStrbuf, "%d", litty.m_cBit);
			}
		}
		grfsew.Clear(FSEW_LiteralSize);
	}
}

void WriteSExpression(Moe::StringBuffer * pStrbuf, STNode * pStnod, SEWK sewk, GRFSEW grfsew)
{
	if (pStnod == nullptr)
	{
		AppendChz(pStrbuf, "null");
		return;
	}

	auto cpStnodChild = pStnod->m_cpStnodChild;
	if (cpStnodChild <= 0)
	{
		WriteStnodFromSewk(pStrbuf, pStnod, sewk, grfsew);
		return;
	}

	AppendChz(pStrbuf, "(");
	WriteStnodFromSewk(pStrbuf, pStnod, sewk, grfsew);

	// don't print trailing null children
	while (cpStnodChild > 0)
	{
		if(pStnod->m_apStnodChild[cpStnodChild-1] == nullptr)
		{
			--cpStnodChild;
		}
		else
		{
			break;
		}
	}

	for (size_t ipStnod = 0; ipStnod < cpStnodChild; ++ipStnod)
	{
		if (MOE_FVERIFY(CBFree(*pStrbuf) > 1, "debug string overflow"))
		{
			*pStrbuf->m_pChzAppend++ = ',';
			*pStrbuf->m_pChzAppend++ = ' ';
		}
		if (MOE_FVERIFY(CBFree(*pStrbuf) > 0, "debug string overflow"))
		{
			STNode * pStnodChild = pStnod->m_apStnodChild[ipStnod];
			WriteSExpression(pStrbuf, pStnodChild, sewk, grfsew);
		}
	}

	if (MOE_FVERIFY(CBFree(*pStrbuf) > 1, "debug string overflow"))
	{
		*pStrbuf->m_pChzAppend++ = ')';
	}

	EnsureTerminated(pStrbuf, '\0');
}


InString IstrSExpression(TypeInfo * pTin, GRFSEW grfsew)
{
	char aCh[1024];
	Moe::StringBuffer strbuf(aCh, MOE_DIM(aCh));

	WriteTypeInfoSExpression(&strbuf, pTin, PARK_Nil);
	return IstrInternCopy(aCh);
}

InString IstrSExpression(STNode * pStnod, SEWK sewk, GRFSEW grfsew)
{
	char aCh[1024];
	Moe::StringBuffer strbuf(aCh, MOE_DIM(aCh));

	WriteSExpression(&strbuf, pStnod, sewk, grfsew);
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

// Symbol lookup rules are as follows 
// Types symbols are unordered and visible anywhere within the symbol table parent hierarchy
// Instances are ordered and only visible within the current nesting level or the global one

enum TABVIS
{
	TABVIS_Unordered,		// all symbols are visible
	TABVIS_Ordered,			// instances are visible, if they come later lexically
	TABVIS_NoInstances,		// no instances, only types are visible
};

static inline TABVIS TabvisCompute(SymbolTable * pSymtabCur, s32 iNestingDepthQuery, GRFSYMLOOK grfsymlook)
{
	if ((pSymtabCur->m_iNestingDepth == 0) | (grfsymlook.FIsSet(FSYMLOOK_IgnoreOrder)))
		return TABVIS_Unordered;

	if (pSymtabCur->m_iNestingDepth < iNestingDepthQuery)
		return TABVIS_NoInstances;
	return TABVIS_Ordered;
}

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

void SymbolTable::AddManagedTin(TypeInfo * pTin)
{
	m_arypTinManaged.Append(pTin);
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

Symbol * SymbolTable::PSymLookup(InString istr, const LexSpan & lexsp, GRFSYMLOOK grfsymlook, SymbolTable ** ppSymtabOut)
{
	if (ppSymtabOut)
		*ppSymtabOut = this;

	if (grfsymlook.FIsSet(FSYMLOOK_Local))
	{
		Symbol ** ppSym = m_hashIstrPSym.Lookup(istr);
		if (ppSym)
		{
			auto tabvis = TabvisCompute(this, m_iNestingDepth, grfsymlook);
			Symbol * pSym = *ppSym;

			while (pSym)
			{
				LexSpan lexspSym = (pSym->m_pStnodDefinition) ? pSym->m_pStnodDefinition->m_lexsp : LexSpan();
				bool fVisibleWhenNested = pSym->m_grfsym.FIsSet(FSYM_VisibleWhenNested);
				if (fVisibleWhenNested | (tabvis == TABVIS_Unordered) | ((tabvis < TABVIS_NoInstances) & (lexspSym <= lexsp)))
				{
					return pSym;
				}
				pSym = pSym->m_pSymPrev;
			}
		}
	}

	SymbolTable * pSymtab = (grfsymlook.FIsSet(FSYMLOOK_Ancestors)) ? m_pSymtabParent : nullptr;
	LexSpan lexspChild = lexsp;
	while (pSymtab)
	{
		Symbol ** ppSym = pSymtab->m_hashIstrPSym.Lookup(istr);

		if (ppSym)
		{
			auto tabvis = TabvisCompute(pSymtab, m_iNestingDepth, grfsymlook);
			Symbol * pSym = *ppSym;
			while (pSym)
			{
				LexSpan lexspSym = (pSym->m_pStnodDefinition) ? pSym->m_pStnodDefinition->m_lexsp : LexSpan();
				bool fVisibleWhenNested = pSym->m_grfsym.FIsSet(FSYM_VisibleWhenNested);
				if (fVisibleWhenNested | (tabvis == TABVIS_Unordered) | ((tabvis < TABVIS_NoInstances) & (lexspSym <= lexsp)))
				{
					if (ppSymtabOut)
						*ppSymtabOut = pSymtab;
					return pSym;
				}
				pSym = pSym->m_pSymPrev;
			}
		}

		pSymtab = pSymtab->m_pSymtabParent;
	}
	return nullptr; 
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

TypeInfoEnum * SymbolTable::PTinenumAllocate(Moe::InString istrName, int cConstant, ENUMK enumk, STEnum * pStenumDef)
{
	size_t cBAlloc = CBAlign(sizeof(TypeInfoEnum), MOE_ALIGN_OF(TypeInfoEnumConstant)) + 
					cConstant * sizeof(TypeInfoEnumConstant);
	u8 * pB = (u8 *)m_pAlloc->MOE_ALLOC(cBAlloc, 8);

	TypeInfoEnum * pTinenum = new(pB) TypeInfoEnum(istrName);
	pTinenum->m_enumk = enumk;

	auto aTinecon = (TypeInfoEnumConstant *)PVAlign( pB + sizeof(TypeInfoEnum), MOE_ALIGN_OF(TypeInfoEnumConstant));
	pTinenum->m_aryTinecon.SetArray(aTinecon, 0, cConstant);

	pTinenum->m_tinstructProduced.m_pStnodStruct = pStenumDef;
	AddManagedTin(pTinenum);

	return pTinenum;
}

TypeInfoProcedure * SymbolTable::PTinprocAllocate(Moe::InString istrName, size_t cParam, size_t cReturn)
{
	size_t cBAlloc = CBAlign(sizeof(TypeInfoProcedure), MOE_ALIGN_OF(TypeInfo *));
	cBAlloc = cBAlloc +	(cParam + cReturn) * sizeof(TypeInfo *) + (cParam * sizeof(GRFPARMQ));

	u8 * pB = (u8 *)m_pAlloc->MOE_ALLOC(cBAlloc,8);
	TypeInfoProcedure * pTinproc = new(pB) TypeInfoProcedure(istrName);
	TypeInfo ** ppTin = (TypeInfo**)PVAlign( pB + sizeof(TypeInfoProcedure), MOE_ALIGN_OF(TypeInfo *));

	pTinproc->m_arypTinParams.SetArray(ppTin, 0, cParam);
	pTinproc->m_arypTinReturns.SetArray(&ppTin[cParam], 0, cReturn);

	auto pGrfparmq = (GRFPARMQ*)&ppTin[cParam + cReturn];
	pTinproc->m_mpIptinGrfparmq.SetArray(pGrfparmq, cParam, cParam);
	ZeroAB(pTinproc->m_mpIptinGrfparmq.A(), pTinproc->m_mpIptinGrfparmq.C() * sizeof(GRFPARMQ));

	AddManagedTin(pTinproc);
	return pTinproc;
}

TypeInfoStruct * SymbolTable::PTinstructAllocate(InString istrIdent, size_t cField, size_t cGenericParam)
{

	size_t cBAlloc = CBAlign(sizeof(TypeInfoStruct), MOE_ALIGN_OF(TypeStructMember)) + 
					cField * sizeof(TypeStructMember) + 
					cGenericParam  * sizeof (TypeInfo *);
	u8 * pB = (u8 *)m_pAlloc->MOE_ALLOC(cBAlloc, 8);

	TypeInfoStruct * pTinstruct = new(pB) TypeInfoStruct(istrIdent);
	AddManagedTin(pTinstruct);

	auto aTypememb = (TypeStructMember*)PVAlign(
											pB + sizeof(TypeInfoStruct), 
											MOE_ALIGN_OF(TypeStructMember));
	pTinstruct->m_aryTypemembField.SetArray(aTypememb, 0, cField);

	return pTinstruct;
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
		return MOE_NEW(pAlloc, STNode) STNode(STEXK_Node, park, lexsp);
	}
};

template <typename T>
T * PStnodAlloc(Moe::Alloc * pAlloc, PARK park, Lexer * pLex, const LexSpan & lexsp)
{
	T * pStnod = StexAlloc<T>::PStnodAlloc(pAlloc, park, lexsp);

	pStnod->m_tok = TOK(pLex->m_tok);
	return pStnod;
}

STNode::~STNode()
{
	MOE_ASSERT(m_cpStnodChild == 0 && m_apStnodChild == nullptr, "failed to cleanup STNode");
}

void CleanupStnodeRecursive(Moe::Alloc * pAlloc, STNode * pStnod)
{
	if (pStnod->m_cpStnodChild)
	{
		for (size_t ipStnod = pStnod->m_cpStnodChild; ipStnod-- > 0; )
		{
			STNode * pStnodChild = pStnod->m_apStnodChild[ipStnod];
			if (pStnodChild)
			{
				CleanupStnodeRecursive(pAlloc, pStnodChild);
				pAlloc->MOE_DELETE(pStnodChild);
			}
		}

		if (pStnod->m_grfstnod.FIsSet(FSTNOD_ChildArrayOnHeap))
		{
			pAlloc->MOE_DELETE(pStnod->m_apStnodChild);
			pStnod->m_grfstnod.Clear(FSTNOD_ChildArrayOnHeap);
		}

		pStnod->m_cpStnodChild = 0;
		pStnod->m_apStnodChild = nullptr;
	}
}

STNode * STNode::PStnodChildSafe(int ipStnod)
{
	if (ipStnod < 0 || ipStnod >= m_cpStnodChild)
		return nullptr;

	return m_apStnodChild[ipStnod];
}

void STNode::SetChildArray(STNode ** apStnodChild, int cpStnodChild)
{
	MOE_ASSERT(m_apStnodChild == nullptr, "leaking child array");

	m_apStnodChild = (STNode **)apStnodChild;
	m_cpStnodChild = cpStnodChild;
}

void STNode::CopyChildArray(Moe::Alloc * pAlloc, STNode ** apStnodChild, int cpStnodChild)
{
	MOE_ASSERT(m_apStnodChild == nullptr, "leaking child array");
	if (!cpStnodChild)
		return;

	size_t cB = sizeof(STNode *) * cpStnodChild;
	m_apStnodChild = (STNode **)pAlloc->MOE_ALLOC(cB, MOE_ALIGN_OF(STNode *));
	m_cpStnodChild = cpStnodChild;
	memcpy(m_apStnodChild, apStnodChild, cB);
	m_grfstnod.AddFlags(FSTNOD_ChildArrayOnHeap);
}

void STNode::CopyChildArray(Moe::Alloc * pAlloc, STNode * pStnodChild)
{
	MOE_ASSERT(m_apStnodChild == nullptr, "leaking child array");

	m_cpStnodChild = 1;
	size_t cB = sizeof(STNode *) * m_cpStnodChild;
	m_apStnodChild = (STNode **)pAlloc->MOE_ALLOC(cB, MOE_ALIGN_OF(STNode *));
	memcpy(m_apStnodChild, &pStnodChild, cB);

	m_grfstnod.AddFlags(FSTNOD_ChildArrayOnHeap);
}

void STNode::AppendChildToArray(Moe::Alloc * pAlloc, STNode * pStnodChild)
{
	STNode ** apStnodPrev = m_apStnodChild;

	++m_cpStnodChild;
	size_t cB = sizeof(STNode *) * m_cpStnodChild;
	m_apStnodChild = (STNode **)pAlloc->MOE_ALLOC(cB, MOE_ALIGN_OF(STNode *));
	memcpy(m_apStnodChild, &pStnodChild, sizeof(STNode *) * (m_cpStnodChild - 1));
	m_apStnodChild[m_cpStnodChild - 1] = pStnodChild;

	if (apStnodPrev && m_grfstnod.FIsSet(FSTNOD_ChildArrayOnHeap))
	{
		pAlloc->MOE_DELETE(apStnodPrev);
	}

	m_grfstnod.AddFlags(FSTNOD_ChildArrayOnHeap);
}

bool STNode::FCheckIsValid(ErrorManager * pErrman)
{
	STEXK stexk = Stexk();
	switch (stexk)
	{
	case STEXK_Node:		break;
	case STEXK_For:			return ((STFor*)this)->FCheckIsValid(pErrman);
	case STEXK_Decl:		return ((STDecl*)this)->FCheckIsValid(pErrman);
	case STEXK_Enum:		return ((STEnum*)this)->FCheckIsValid(pErrman);
	case STEXK_Struct:		return ((STStruct*)this)->FCheckIsValid(pErrman);
	case STEXK_Proc:		return ((STProc*)this)->FCheckIsValid(pErrman);
	case STEXK_Value:		return ((STValue*)this)->FCheckIsValid(pErrman);
	case STEXK_Operator:	return ((STOperator*)this)->FCheckIsValid(pErrman);
	default:
		MOE_ASSERT(false, "missing assert valid for STNode type");
		break;
	}

	return true;
}

bool STFor::FCheckIsValid(ErrorManager * pErrman)
{
	MOE_ASSERT(Stexk() == STEXK_For, "bad park mapping");
	return true;
}

bool STDecl::FCheckIsValid(ErrorManager * pErrman)
{
#ifdef MOEB_LATER 
	- notes need to be revised affter we have a new compound decl approach

	// Decl structure has become a bit complicated...
	// ParameterList { Decl, Decl, Decl{ childDecl[3] } }
	// ParameterLists contain declarations and declarations can contain compound (ie parent/child declarations)
	//   Compound declarations only allow one level deep and are there to support comma separated declarations 
	//   and initialization to multiple return types.

	// n1, n2:s32, g1: f32;	// would be one compound decl
	// parent decls cannot have a type.
	// child decls cannot have initializers - the initializer should come from the parent.
#endif

	MOE_ASSERT(Stexk() == STEXK_Decl, "bad park mapping");

	bool fMissingTypeSpecifier;
	bool fIsValid = true;
	if (1) //m_pStnodChildMin == nullptr)
	{
		fMissingTypeSpecifier = (m_pStnodType == nullptr);
	}
	else // compound decl
	{
#ifdef MOEB_LATER
		if (pStdecl->m_iStnodType != -1)
			ParseError(pParctx, pLex, "Internal error, compound decl should not specify a type");

		fMissingTypeSpecifier = false;
		for (int iStnodChild = pStdecl->m_iStnodChildMin; iStnodChild != pStdecl->m_iStnodChildMax; ++iStnodChild)
		{
			auto pStdeclChild = PStmapDerivedCast<CSTDecl *>(pStnodDecl->PStnodChild(iStnodChild)->m_pStmap);
			MOE_ASSERT(pStdeclChild->m_iStnodIdentifier != -1, "declaration missing identifier");

			if (pStdeclChild->m_iStnodInit != -1)
				ParseError(pParctx, pLex, "Internal error, child decl should not specify an initializer");

			MOE_ASSERT(pStdeclChild->m_iStnodChildMin == -1 && pStdeclChild->m_iStnodChildMax == -1, "nested children not supported");

			fMissingTypeSpecifier |= (pStdeclChild->m_iStnodType == -1);
		}
#endif
	}
	
	//auto pStnodInit = pStnodDecl->PStnodChildSafe(pStdecl->m_iStnodInit);
	if (fMissingTypeSpecifier & (m_pStnodInit == nullptr))
	{
		EmitError(pErrman, m_lexsp, ERRID_TypeSpecifierExpected, "Expected type specifier or initialization");
	}
	if (m_pStnodInit && m_pStnodInit->m_park == PARK_Uninitializer)
	{
		if (fMissingTypeSpecifier)
		{
			EmitError(pErrman, m_lexsp, ERRID_UninitializerNotAllowed, "Uninitializer not allowed without specified type");
			fIsValid = false;
		}
	}

	return fIsValid;
}

bool STEnum::FCheckIsValid(ErrorManager * pErrman)
{
	MOE_ASSERT(Stexk() == STEXK_Enum, "bad park mapping");
	return true;
}

bool STStruct::FCheckIsValid(ErrorManager * pErrman)
{
	MOE_ASSERT(Stexk() == STEXK_Struct, "bad park mapping");
	return true;
}

bool STProc::FCheckIsValid(ErrorManager * pErrman)
{
	MOE_ASSERT(Stexk() == STEXK_Proc, "bad park mapping");
	return true;
}

bool STValue::FCheckIsValid(ErrorManager * pErrman)
{
	MOE_ASSERT(Stexk() == STEXK_Value, "bad park mapping");
	return true;
}

bool STOperator::FCheckIsValid(ErrorManager * pErrman)
{
	MOE_ASSERT(Stexk() == STEXK_Operator, "bad park mapping");
	return true;
}

static inline bool FIsValidStnod(STNode * pStnod)
{
	return pStnod && pStnod->m_park != PARK_Error;
}

bool FExpectConsumeToken(ParseContext * pParctx, Lexer * pLex, TOK tokExpected, const char * pChzInfo = nullptr, ...)
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
	if (!FIsEndOfStatement(pLex))
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
		(void) TokSkipToRecovery(pLex, pParctx->m_pLrecst);
	}

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
}
STNode * PStnodParsePointerDecl(ParseContext * pParctx, Lexer * pLex)
{
	// handle the mis-lexing of '&&' as one token here
	if (pLex->m_tok == TOK_DoubleReference)
	{
		SplitToken(pLex, TOK_Reference);
	}

	if (FTryConsumeToken(pLex, TOK_Reference))
	{
		auto pStnod = PStnodAlloc<STNode>(pParctx->m_pAlloc, PARK_ReferenceDecl, pLex, LexSpan(pLex));
		return pStnod;
	}

	return nullptr;
}

STNode * PStnodParseQualifierDecl(ParseContext * pParctx, Lexer * pLex)
{
	if (pLex->m_tok == TOK_Identifier && (pLex->m_istr == RWord::g_istrConst || pLex->m_istr == RWord::g_istrInArg))
	{
		auto pStval = PStnodAlloc<STValue>(pParctx->m_pAlloc, PARK_QualifierDecl, pLex, LexSpan(pLex));
		pStval->SetIstr(pLex->m_istr);

		TokNext(pLex);	
		return pStval;
	}

	return nullptr;
}

STNode * PStnodParseArrayDecl(ParseContext * pParctx, Lexer * pLex, GRFPDECL grfpdecl)
{
	if (FTryConsumeToken(pLex, TOK('[')))
	{
		auto pStnodArray = PStnodAlloc<STNode>(pParctx->m_pAlloc, PARK_ArrayDecl, pLex, LexSpan(pLex));

		if (FTryConsumeToken(pLex, TOK_PeriodPeriod))
		{
			;
		}
		else if (pLex->m_tok != TOK(']'))
		{
#ifdef MOEB_LATER
			STNode * pStnodDim = PStnodParseBakedConstant(pParctx, pLex, PARK_Decl);
#else
			STNode * pStnodDim = nullptr;
#endif
			if (pStnodDim)
			{
				auto pSymtab = pParctx->m_pSymtab;

				// BB - do we need to spoof a pStnodType here?
				pStnodDim->m_pTin = pSymtab->PTinBuiltin(IstrIntern(BuiltIn::g_pChzInt));
			}
			else
			{
				pStnodDim = PStnodParseExpression(pParctx, pLex);
			}

			if (pStnodDim)
			{
				pStnodArray->CopyChildArray(pParctx->m_pAlloc, pStnodDim);
			}
		}

		FExpectConsumeToken(pParctx, pLex, TOK(']'));
		return pStnodArray;
	}
	return nullptr;
}

STNode * PStnodParseTypeSpecifier(ParseContext * pParctx, Lexer * pLex, const char * pChzErrorContext, GRFPDECL grfpdecl)
{
	STNode * pStnod = PStvalParseIdentifier(pParctx, pLex);
	if (pStnod)
	{
		if (FTryConsumeToken(pLex, TOK('(')))
		{
			STNode * pStnodStructInst = nullptr;
			{
				LexRecoverAmbit lrecamb(pParctx->m_pLrecst, TOK(')'));
				SymbolTable * pSymtabParent = pParctx->m_pSymtab;

				pStnodStructInst = PStnodAlloc<STNode>(pParctx->m_pAlloc, PARK_GenericStructSpec, pLex, LexSpan(pLex));
				
				CDynAry<STNode *> arypStnodArg(pParctx->m_pAlloc, BK_Parse);
				arypStnodArg.Append(pStnod);

				ParseArgumentList(pParctx, pLex, &arypStnodArg, FARGLIST_AllowGenericValues);
				pStnodStructInst->CopyChildArray(pParctx->m_pAlloc, arypStnodArg.A(), int(arypStnodArg.C()));
			}

			if (!FExpectConsumeToken(pParctx, pLex, TOK(')')))
			{
				(void)TokSkipToRecovery(pLex, pParctx->m_pLrecst);
			}
			return pStnodStructInst;

		}
		else if (pLex->m_tok == TOK('.'))
		{
			while (FTryConsumeToken(pLex, TOK('.')))
			{
				LexSpan lexsp(pLex);

				TOK tokPrev = TOK(pLex->m_tok);
				auto pStvalIdent = PStvalParseIdentifier(pParctx, pLex);
				if (!pStvalIdent)
				{
					EmitError(pParctx, lexsp, ERRID_MissingIdentifier, "Expected identifier after '.' before %s", PChzFromTok(tokPrev));
				}
				else
				{
					auto pStop = PStnodAlloc<STOperator>(pParctx->m_pAlloc, PARK_MemberLookup, pLex, lexsp);
					pStop->m_tok = TOK('.');
					pStop->m_pStnodLhs = pStnod;
					pStop->m_pStnodRhs = pStvalIdent;
					pStnod = pStop;
				}
			}
		}
		return pStnod;
	}

#ifdef MOEB_LATER
	pStnod = PStnodParseProcedureReferenceDecl(pParctx, pLex);
	if (pStnod)
		return pStnod;

	pStnod = PStnodParseGenericTypeDecl(pParctx, pLex, grfpdecl);
	if (pStnod)
		return pStnod;
#endif

	pStnod = PStnodParsePointerDecl(pParctx, pLex);
	if (!pStnod)
	{
		pStnod = PStnodParseQualifierDecl(pParctx, pLex);
	}

	if (!pStnod)
	{
		pStnod = PStnodParseArrayDecl(pParctx, pLex, grfpdecl);
	}
	if (!pStnod)
		return nullptr;

	STNode * pStnodChild = PStnodParseTypeSpecifier(pParctx, pLex, pChzErrorContext, grfpdecl);
	if (!pStnodChild)
	{
		EmitError(pParctx, LexSpan(pLex), ERRID_TypeSpecParseFail, "Expected type type name before '%s'", PChzFromTok(TOK(pLex->m_tok)));
	}
	else
	{
		MOE_ASSERT(!pStnod->FHasChildArray(), "child array should be empty");
		pStnod->CopyChildArray(pParctx->m_pAlloc, pStnodChild);

#ifdef MOEB_NO_TYPES_IN_PARSE
		if (auto pTinptr = PTinRtiCast<TypeInfoPointer *>(pStnod->m_pTin))
		{
			pTinptr->m_pTin = pStnodChild->m_pTin;
		}
		else if (auto pTinary = PTinRtiCast<TypeInfoArray *>(pStnod->m_pTin))
		{
			pTinary->m_pTin = pStnodChild->m_pTin;
		}
#endif
	}

	return pStnod;
}

STValue * PStvalAllocateIdentifier(ParseContext * pParctx, Lexer * pLex, const LexSpan & lexsp, const Moe::InString istrIdent)
{
	STValue * pStval = PStnodAlloc<STValue>(pParctx->m_pAlloc, PARK_Identifier, pLex, LexSpan(pLex));
	pStval->m_istr = istrIdent;

	return pStval;
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
	(void) pStval->FCheckIsValid(pParctx->PErrman());

	if (istrIdent.m_pChz[0] == '#')
	{
		EmitError(pParctx, LexSpan(pLex), ERRID_UnknownDirective, "Unknown directive encountered %s", istrIdent.m_pChz);
	}

	TokNext(pLex);
	return pStval;
}

// BB - should merge parseIdentifier and parseReservedWord
STValue * PStvalParseReservedWord(ParseContext * pParctx, Lexer * pLex, Moe::InString istrRwordExpected = Moe::InString())
{
	if (pLex->m_tok != TOK_Identifier || !FIsReservedWord(pLex->m_istr))
		return nullptr;

	if (!istrRwordExpected.FIsNull() && pLex->m_istr != istrRwordExpected)
	{
		EmitError(pParctx, LexSpan(pLex), ERRID_MissingRWord, "Expected %s before %s", istrRwordExpected.m_pChz, pLex->m_istr.m_pChz);
		return nullptr;
	}

	STValue * pStval = PStnodAlloc<STValue>(pParctx->m_pAlloc, PARK_ReservedWord, pLex, LexSpan(pLex));
	pStval->SetIstr(pLex->m_istr);
	(void)pStval->FCheckIsValid(pParctx->PErrman());
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
	GRFEXP grfexp,
	const char * pChzContext,
	TOK tokClosing)
{
	TOK aTok[] = {TOK(' '), TOK(',') };
	aTok[0] = tokClosing;
	auto pLrec = PLrecPush(pParctx->m_pLrecst, aTok);

	LexSpan lexsp(pLex);

	STNode * pStnodExp = PStnodParseExpression(pParctx, pLex, grfexp);
	STNode * pStnodList = nullptr;

	if (pStnodExp)
	{
		pStnodList = PStnodAlloc<STNode>(pParctx->m_pAlloc, PARK_ExpressionList, pLex, lexsp);

		CDynAry<STNode *> arypStnod(pParctx->m_pAlloc, BK_Parse);
		arypStnod.Append(pStnodExp);

		while (FTryConsumeToken(pLex, TOK(',')))
		{
			pStnodExp = PStnodParseExpression(pParctx, pLex, grfexp);

			if (!pStnodExp)
			{
				EmitError(pParctx, LexSpan(pLex), ERRID_ExpectedExpression, "Expected expression before %s %s", PChzCurrentToken(pLex), pChzContext);
				break;
			}
			arypStnod.Append(pStnodExp);
		}

		pStnodList->CopyChildArray(pParctx->m_pAlloc, arypStnod.A(), int(arypStnod.C()));
	}

	PopLexRecover(pParctx->m_pLrecst, pLrec);

	if (!FExpectConsumeToken(pParctx, pLex, TOK('}'), pChzContext))
	{
		(void)TokSkipToRecovery(pLex, pParctx->m_pLrecst);
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
				if (FIsReservedWord(pLex->m_istr))
				{
					if (pLex->m_istr == RWord::g_istrTrue)
					{
						pStval = PStvalParseReservedWord(pParctx, pLex);
						//pStval->SetU64(true);
					}
					else if (pLex->m_istr == RWord::g_istrFalse)
					{
						pStval = PStvalParseReservedWord(pParctx, pLex);
						//pStval->SetU64(false);
					}
					else if (pLex->m_istr == RWord::g_istrNull)
					{
						pStval = PStvalParseReservedWord(pParctx, pLex);
						//pStval->SetU64(u32(0));
					}
					else if (pLex->m_istr == RWord::g_istrFileDirective)
					{
						pStval = PStvalParseReservedWord(pParctx, pLex);

						auto pStvalChild = PStnodAlloc<STValue>(pParctx->m_pAlloc, PARK_Literal, pLex, pStval->m_lexsp);
						pStvalChild->SetIstr(pStval->m_lexsp.m_istrFilename);
					}
					else if (pLex->m_istr == RWord::g_istrLineDirective)
					{
						pStval = PStvalParseReservedWord(pParctx, pLex);

						LexLookup lexlook(pParctx->m_pWork, pStval);

						auto pStvalChild = PStnodAlloc<STValue>(pParctx->m_pAlloc, PARK_Literal, pLex, pStval->m_lexsp);
						pStvalChild->SetS64(lexlook.m_iLine);
					}
					else
					{
						return nullptr;
					}
				}
				else
				{
					pStval = PStvalParseIdentifier(pParctx, pLex);

					if (FTryConsumeToken(pLex, TOK_ColonColon))
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
						} while (FTryConsumeToken(pLex, TOK(',')));

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

				if (FTryConsumeToken(pLex, TOK(':')))
				{
					// parse type specifier
					pStnodType = PStnodParseTypeSpecifier(pParctx, pLex, "compound literal", FPDECL_AllowBakedTypes | FPDECL_AllowBakedValues);

					if (!FIsValidStnod(pStnodType))
					{
						EmitError(pParctx, lexsp, ERRID_TypeSpecParseFail, "expected type specification following ':'");
						return pStnodType;
					}
				}

				if (FTryConsumeToken(pLex, TOK('{')))
				{
					// We're using a decl here... may need a custom structure
					auto pStdeclLit = PStnodAlloc<STDecl>(pParctx->m_pAlloc, PARK_CompoundLiteral, pLex, lexsp);

					pStdeclLit->m_pStnodType = pStnodType;

					STNode * pStnodValues = PStnodParseExpressionList(pParctx, pLex, FEXP_AllowLiteralMemberLabel, "while parsing struct/array literal", TOK('}'));
					pStdeclLit->m_pStnodInit = pStnodValues;
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

				LexRecoverAmbit lrecamb(pParctx->m_pLrecst, TOK(')'), TOK_EndOfLine);

				STNode * pStnodReturn = PStnodParseExpression(pParctx, pLex);
				FExpectConsumeToken(pParctx, pLex, TOK(')'));
				return pStnodReturn;
			}

		default: return nullptr;
	}
}

void ParseArgumentList(ParseContext * pParctx, Lexer * pLex, CDynAry<STNode *> * parypStnodArgList, GRFARGLIST grfarglist)
{
	static const TOK s_aTok[] = {TOK(','), TOK(')'), TOK_EndOfLine };
	LexRecoverAmbit lrecamb(pParctx->m_pLrecst, s_aTok);

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
					(void)TokSkipToRecovery(pLex, pParctx->m_pLrecst);
				}
				else
				{
					pChzLabel = IstrFromIdentifier(pStnodIdent).m_pChz;
					arypStnodLabel.Append(pStnodIdent);
				}

				if (FTryConsumeToken(pLex, TOK('=')))
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
					(void)TokSkipToRecovery(pLex, pParctx->m_pLrecst);
				}
				else
				{
					arypStnodLabel.Append(pStnodArg);
					pStnodLabel->CopyChildArray(pParctx->m_pAlloc, arypStnodLabel.A(), int(arypStnodLabel.C()));
					pStnodArg = pStnodLabel;
				}
			}

			parypStnodArgList->Append(pStnodArg);

			if ((pStnodArg == nullptr))
				break;
		}

		if (!FTryConsumeToken(pLex, TOK(',')))
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
				LexRecoverAmbit(pParctx->m_pLrecst, TOK(']'), TOK_EndOfLine);
				LexSpan lexsp(pLex);

				// BB - Need to push a 'bail out' context... in case of error walk to next ']'
				TokNext(pLex); // consume '['

				auto pStnodArray = PStnodAlloc<STNode>(pParctx->m_pAlloc, PARK_ArrayElement, pLex, lexsp);

				STNode * pStnodElement = PStnodParseExpression(pParctx, pLex);

				STNode * apStnod[] = {pStnod, pStnodElement};
				pStnodArray->CopyChildArray(pParctx->m_pAlloc, apStnod, MOE_DIM(apStnod));

				pStnod = pStnodArray;
				FExpectConsumeToken(pParctx, pLex, TOK(']'));
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
				CDynAry<STNode *> arypStnodArg(pParctx->m_pAlloc, BK_Parse);
				arypStnodArg.Append(pStnod);

				pStnod = pStnodCall;

				// parsing this with LogicalAndOrExpression even though ISO c uses assignmentExpression
				//  need to change this if we expect assignments to return the assigned value (x := a = b; )

				ParseArgumentList(pParctx, pLex, &arypStnodArg);
				pStnod->CopyChildArray(pParctx->m_pAlloc, arypStnodArg.A(), int(arypStnodArg.C()));

				if (!FExpectConsumeToken(
					pParctx,
					pLex,
					TOK(')'),
					"while parsing procedure call '%s'", 
					pStnodIdent ? IstrFromIdentifier(pStnodIdent).m_pChz : "unknown"))
				{
					// This is a comma-separated argument list, let's assume we missed a comma and the rest of the statement is garbage
					(void)TokSkipToRecovery(pLex, pParctx->m_pLrecst);
				}
			} 
			break;
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
			if (pLex->m_istr == RWord::g_istrSizeof || 
				pLex->m_istr == RWord::g_istrAlignof || 
				pLex->m_istr == RWord::g_istrTypeinfo || 
				pLex->m_istr == RWord::g_istrTypeof)
			{
				TOK tokPrev = TOK(pLex->m_tok);	
				auto pStvalRword = PStvalParseReservedWord(pParctx, pLex);

				bool fIsOk = FExpectConsumeToken(pParctx, pLex, TOK('('));

				STNode * pStnodChild = PStnodParseUnaryExpression(pParctx, pLex);
				if (!pStnodChild)
				{
					EmitError(pParctx, LexSpan(pLex), ERRID_MissingOperand, "%s missing argument.", pLex->m_istr.m_pChz);
					(void)TokSkipToRecovery(pLex, pParctx->m_pLrecst);
					fIsOk = false;
				}
				
				fIsOk &= FExpectConsumeToken(pParctx, pLex, TOK(')'));
					
				if (pLex->m_istr == RWord::g_istrTypeof)
				{
					EmitError(pParctx, LexSpan(pLex), ERRID_FeatureNotImplemented, "typeof not implemented yet.");
					(void)TokSkipToRecovery(pLex, pParctx->m_pLrecst);
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
				(void)TokSkipToRecovery(pLex, pParctx->m_pLrecst);
				return nullptr;
			}

			STOperator * pStop = PStnodAlloc<STOperator>(pParctx->m_pAlloc, PARK_UnaryOp, pLex, lexsp);
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
	if (pLex->m_istr != RWord::g_istrCast)
	{
		return PStnodParseUnaryExpression(pParctx, pLex);
	}

	TokNext(pLex);

	auto pStdecl = PStnodAlloc<STDecl>(pParctx->m_pAlloc, PARK_Cast, pLex, LexSpan(pLex));
	//auto pStdecl = pStnodCast->PStmapEnsure<CSTDecl>(pParctx->m_pAlloc);

	FExpectConsumeToken(pParctx, pLex, TOK('('));

	pStdecl->m_pStnodType = PStnodParseTypeSpecifier(pParctx, pLex, "cast", FPDECL_None);

	FExpectConsumeToken(pParctx, pLex, TOK(')'));

	pStdecl->m_pStnodInit = PStnodParseCastExpression(pParctx, pLex);
	//pStdecl->m_pStnodInit = pStnodCast->IAppendChild(pStnodChild);

	if (pStdecl->m_pStnodInit == nullptr)
	{
		EmitError(pParctx, pStnodCast->m_lexsp, ERRID_UnknownError, "Cast statement missing right hand side");
		(void)TokSkipToRecovery(pLex, pParctx->m_pLrecst);
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

		(void)TokSkipToRecovery(pLex, pParctx->m_pLrecst);
		return pStnodLhs;
	}

	STOperator * pStopExp = PStnodAlloc<STOperator>(pParctx->m_pAlloc, parkExpression, pLex, LexSpan(pLex));
	pStopExp->m_tok = tokExpression;
	pStopExp->m_pStnodLhs = pStnodLhs;
	pStopExp->m_pStnodRhs = pStnodRhs;
	return pStopExp;
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

STNode * PStnodParseExpressionStatement(ParseContext * pParctx, Lexer * pLex)
{
	if (FTryConsumeToken(pLex, TOK(';')))
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

	if (FTryConsumeToken(pLex, TOK('{')))
	{
		CDynAry<STNode *> arypStnod(pParctx->m_pAlloc, BK_Parse);
		pStnodList = PStnodAlloc<STNode>(pParctx->m_pAlloc, PARK_List, pLex, LexSpan(pLex));
		if (!pSymtab)
		{
			pSymtab = PSymtabNew(pParctx->m_pAlloc, pParctx->m_pSymtab, IstrIntern("anon"));
		}

		pStnodList->m_pSymtab = pSymtab;
		PushSymbolTable(pParctx, pSymtab);

		static const TOK s_aTok[] = {TOK(';'), TOK('}'), TOK_EndOfLine };
		LexRecoverAmbit lrecamb(pParctx->m_pLrecst, s_aTok);

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

		pStnodList->CopyChildArray(pParctx->m_pAlloc, arypStnod.A(), int(arypStnod.C()));

		SymbolTable * pSymtabPop = PSymtabPop(pParctx);
		MOE_ASSERT(pSymtab == pSymtabPop, "CSymbol table push/pop mismatch (list)");
		FExpectConsumeToken(pParctx, pLex, TOK('}'));
	}

	return pStnodList;
}

STNode * PStnodParseParameter(
	ParseContext * pParctx,
	Lexer * pLex,
	SymbolTable * pSymtab,
	GRFPDECL grfpdecl)
{

	LexSpan lexsp(pLex);
	if (pLex->m_tok == TOK_PeriodPeriod && grfpdecl.FIsSet(FPDECL_AllowVariadic))
	{
		TokNext(pLex);

		auto pStnodVarArgs = PStnodAlloc<STNode>(pParctx->m_pAlloc, PARK_VariadicArg, pLex, lexsp);
		pStnodVarArgs->m_tok = TOK_PeriodPeriod;
		return pStnodVarArgs;
	}

#if MOEB_LATER
	auto pStnodUsing = PStnodParseUsingStatement(pParctx, pLex, pSymtab);
#else
	STNode * pStnodUsing = nullptr;
#endif
	if (pStnodUsing)
	{
		if (grfpdecl.FIsSet(FPDECL_AllowUsing))
		{
			return pStnodUsing;
		}

		EmitError(pParctx, lexsp, ERRID_UsingStatementNotAllowed, "Using statement not allowed in this context");
		pParctx->m_pAlloc->MOE_DELETE(pStnodUsing);
		return nullptr;
	}

	STNode * pStnodReturn = nullptr;
	STNode * pStnodCompound = nullptr;
	STNode * pStnodInit = nullptr;
	bool fAllowCompoundDecl = grfpdecl.FIsSet(FPDECL_AllowCompoundDecl);
	bool fAllowBakedValues = grfpdecl.FIsSet(FPDECL_AllowBakedValues);
	bool fAllowConstants = grfpdecl.FIsSet(FPDECL_AllowConstants);
	bool fIsUnnamed = false;

	Lexer lexPeek = *pLex;
	int cIdent = 0;
	while (1)
	{
		if (fAllowBakedValues)
		{
			(void)FTryConsumeToken(&lexPeek, TOK_Generic); // ignore baked constant marks
		}

		if (lexPeek.m_tok != TOK_Identifier)
		{
			if (lexPeek.m_tok == TOK(':') && grfpdecl.FIsSet(FPDECL_AllowUnnamed))
			{
				fIsUnnamed = true;
				break;
			}
			return nullptr;
		}

		InString istrIdent = lexPeek.m_istr;

		++cIdent;
		TokNext(&lexPeek);

		if (fAllowCompoundDecl && FTryConsumeToken(&lexPeek, TOK(',')))
			continue;

		if (FConsumeIdentifier(pLex, RWord::g_istrImmutable))
			break;

		if ((lexPeek.m_tok == TOK(':')) | (lexPeek.m_tok == TOK_ColonEqual))
			break;

		return nullptr;
	}

	int cTypeNeeded = 0;
	STDecl * pStdecl = nullptr;
	do
	{
		bool fIsBakedConstant = false;
		if (fAllowBakedValues && FTryConsumeToken(pLex, TOK_Generic))
		{
			fIsBakedConstant = true;
		}

		if (pStnodInit)
			EmitError(pParctx, LexSpan(pLex), ERRID_CompoundDeclNotAllowed, "Initializer must come after all comma separated declarations");

		STNode * pStnodIdent = nullptr; 
		if (!fIsUnnamed)
		{
			pStnodIdent = PStvalParseIdentifier(pParctx, pLex);
			if (!MOE_FVERIFY(pStnodIdent, "parse failed during decl peek"))
				return nullptr;
		}

		pStdecl = PStnodAlloc<STDecl>(pParctx->m_pAlloc, PARK_Decl, pLex, lexsp);

		//auto pStdecl = pStnodDecl->PStmapEnsure<CSTDecl>(pParctx->m_pAlloc);
		pStdecl->m_fIsBakedConstant = fIsBakedConstant;
		++cTypeNeeded;

		if (pStnodReturn)
		{
#ifdef MOEB_LATER // compound decls need a rewrite

			if (!pStnodCompound)
			{
				pStnodCompound = PStnodAlloc<STDecl>(pParctx->m_pAlloc, PARK_Decl, pLex, lexsp);

				//auto pStdeclCompound = pStnodCompound->PStmapEnsure<CSTDecl>(pParctx->m_pAlloc);

				pStdeclCompound->m_iStnodChildMin = pStnodCompound->IAppendChild(pStnodReturn);
				pStnodReturn = pStnodCompound;
			}

			auto pStdeclCompound = PStmapDerivedCast<CSTDecl *>(pStnodCompound->m_pStmap);
			pStdeclCompound->m_iStnodChildMax = pStnodCompound->IAppendChild(pStnodDecl) + 1;
#else
			EmitError(pParctx, lexsp, ERRID_FeatureNotImplemented, "CompundDecls need rewrite");
#endif
		}
		else
		{
			pStnodReturn = pStdecl;
		}

		if (!fIsUnnamed)
		{
			// NOTE: May not resolve symbols (symtab is null if this is a procedure reference)
			if (pSymtab)
			{
				FSHADOW fshadow = (grfpdecl.FIsSet(FPDECL_AllowShadowing)) ? FSHADOW_ShadowingAllowed : FSHADOW_NoShadowing;
				pStnodIdent->m_pSymbase = pSymtab->PSymEnsure(pParctx->m_pWork->m_pErrman, IstrFromIdentifier(pStnodIdent), pStdecl, FSYM_None, fshadow);
			}

			pStdecl->m_pStnodIdentifier = pStnodIdent;
		}


		if (FConsumeIdentifier(pLex, RWord::g_istrImmutable))
		{
			if (pStnodCompound)
				EmitError(pParctx, LexSpan(pLex), ERRID_CompoundDeclNotAllowed, "Comma separated declarations not supported for immutable values");

			if (!fAllowConstants)
			{
				EmitError(pParctx, LexSpan(pLex), ERRID_ImmutableNotAllowed, "immutable declarations not supported in parameter list");
			}
			else
			{
				pStdecl->m_park = PARK_ConstantDecl;
			}
		}

		if (FTryConsumeToken(pLex, TOK_ColonEqual))
		{
			pStnodInit = PStnodParseExpression(pParctx, pLex);
		}
		else if (FTryConsumeToken(pLex, TOK(':')))
		{
			if (pStnodCompound)
			{
				MOE_ASSERT(cTypeNeeded, "No compound children?");

#ifdef MOEB_LATER
				auto pStnodType = PStnodParseTypeSpecifier(pParctx, pLex, "declaration", grfpdecl);

				auto pStdeclCompound = PStmapDerivedCast<CSTDecl *>(pStnodCompound->m_pStmap);
				int iStnodChild = pStdeclCompound->m_iStnodChildMax - cTypeNeeded;
				int iChild = 0;
				for ( ; iStnodChild < pStdeclCompound->m_iStnodChildMax; ++iStnodChild)
				{
					auto pStnodChild = pStnodCompound->PStnodChild(iStnodChild);

					auto pStdeclChild = PStmapDerivedCast<CSTDecl *>(pStnodChild->m_pStmap);
					MOE_ASSERT(pStdeclChild->m_iStnodType == -1, "shouldn't set the type child twice");

					auto pStnodTypeCopy = (iChild == 0) ? pStnodType : PStnodCopy(pParctx->m_pAlloc, pStnodType);
					++iChild;

					pStdeclChild->m_iStnodType = pStnodChild->IAppendChild(pStnodTypeCopy);
				}
#endif

				cTypeNeeded = 0;
			}
			else
			{
				auto pStnodType = PStnodParseTypeSpecifier(pParctx, pLex, "declaration", grfpdecl);
				pStdecl->m_pStnodType = pStnodType;
				cTypeNeeded = 0;
			}

			if (FTryConsumeToken(pLex, TOK('=')))
			{
				if (FTryConsumeToken(pLex, TOK_TripleMinus))
				{
					if (!grfpdecl.FIsSet(FPDECL_AllowUninitializer))
					{
						EmitError(pParctx, LexSpan(pLex), ERRID_UninitializerNotAllowed, "--- uninitializer not allowed in parameter lists");
					}
					else
					{
						pStnodInit = PStnodAlloc<STNode>(pParctx->m_pAlloc, PARK_Uninitializer, pLex, LexSpan(pLex));
						pStnodInit->m_tok = TOK_TripleMinus;
					}
				}
				else
				{
					pStnodInit = PStnodParseExpression(pParctx, pLex);
					if (!pStnodInit)
						EmitError(pParctx, LexSpan(pLex), ERRID_InitialValueExpected, "initial value expected before %s", PChzCurrentToken(pLex));
				}
			}
		}

	} while (fAllowCompoundDecl && FTryConsumeToken(pLex, TOK(',')));

	auto pStdeclReturn = PStnodDerivedCast<STDecl *>(pStnodReturn);
	pStdeclReturn->m_pStnodInit = pStnodInit;

	//ValidateDeclaration(pParctx, pLex, pStnodReturn, grfpdecl);
	MOE_ASSERT(pStdecl->m_pStnodIdentifier != nullptr || grfpdecl.FIsSet(FPDECL_AllowUnnamed), "declaration missing identifier");
	(void)pStdeclReturn->FCheckIsValid(pParctx->PErrman());
	return pStnodReturn;
}

STNode * PStnodParseDecl(ParseContext * pParctx, Lexer * pLex)
{
	// stand alone declaration statement

	GRFPDECL grfpdecl = FPDECL_AllowUninitializer | FPDECL_AllowCompoundDecl | FPDECL_AllowConstants | FPDECL_AllowUsing | FPDECL_AllowShadowing;
	auto * pStnod =  PStnodParseParameter(pParctx, pLex, pParctx->m_pSymtab, grfpdecl);
	if (!pStnod)
		return nullptr;

	ExpectEndOfStatement(pParctx, pLex);
	return pStnod;
}

struct OverloadInfo // tag = ovinf
{
	const char *	m_pChz;
	TOK				m_tok;
	PARK			m_aPark[2];
};

static const OverloadInfo s_aOvinf[] = {
	{ "operator+", TOK('+'),			{ PARK_AdditiveOp, PARK_UnaryOp} },
	{ "operator-", TOK('-'),			{ PARK_AdditiveOp, PARK_UnaryOp} },
	{ "operator|", TOK('|'),			{ PARK_AdditiveOp, PARK_Nil} },
	{ "operator^", TOK('^'),			{ PARK_AdditiveOp, PARK_Nil} },
	{ "operator*", TOK('*'),			{ PARK_MultiplicativeOp, PARK_Nil} },
	{ "operator/", TOK('/'),			{ PARK_MultiplicativeOp, PARK_Nil} },
	{ "operator%", TOK('%'),			{ PARK_MultiplicativeOp, PARK_Nil} },
	{ "operator&", TOK('&'),			{ PARK_MultiplicativeOp, PARK_UnaryOp} },
	{ "operator<<", TOK_ShiftLeft,		{ PARK_ShiftOp, PARK_Nil} },
	{ "operator>>", TOK_ShiftRight,		{ PARK_ShiftOp, PARK_Nil} },
	{ "operator>", TOK('>'),			{ PARK_RelationalOp, PARK_Nil} },
	{ "operator<", TOK('<'),			{ PARK_RelationalOp, PARK_Nil} },
	{ "operator<=", TOK_LessEqual,		{ PARK_RelationalOp, PARK_Nil} },
	{ "operator>=", TOK_GreaterEqual,	{ PARK_RelationalOp, PARK_Nil} },
	{ "operator==", TOK_EqualEqual,		{ PARK_RelationalOp, PARK_Nil} },
	{ "operator!=", TOK_NotEqual,		{ PARK_RelationalOp, PARK_Nil} },
	{ "operator+=", TOK_PlusEqual,		{ PARK_AssignmentOp, PARK_Nil} },
	{ "operator-=", TOK_MinusEqual,		{ PARK_AssignmentOp, PARK_Nil} },
	{ "operator*=", TOK_MulEqual,		{ PARK_AssignmentOp, PARK_Nil} },
	{ "operator/=", TOK_DivEqual,		{ PARK_AssignmentOp, PARK_Nil} },
	{ "operator=", TOK('='),			{ PARK_AssignmentOp, PARK_Nil} },
	{ "operator:=", TOK_ColonEqual,		{ PARK_Decl, PARK_Nil} },
	{ "operator++", TOK_PlusPlus,		{ PARK_PostfixUnaryOp, PARK_Nil} },
	{ "operator--", TOK_MinusMinus,		{ PARK_PostfixUnaryOp, PARK_Nil} },
	{ "operator@", TOK_Dereference,		{ PARK_UnaryOp, PARK_Nil} },
	{ "operator~", TOK('~'),			{ PARK_UnaryOp, PARK_Nil} },
	{ "operator!", TOK('!'),			{ PARK_UnaryOp, PARK_Nil} },
};

enum FOVSIG
{
	FOVSIG_MustTakeReference	= 0x1,
	FOVSIG_ReturnBool			= 0x2,
	FOVSIG_AllowCommutative		= 0x4,


	FOVSIG_None			= 0x0,
	FOVSIG_All			= 0x7,
};
MOE_DEFINE_GRF(GRFOVSIG, FOVSIG, u32);

struct OverloadSignature // tag=ovsig
{
	PARK			m_park;
	int				m_cParam;
	int				m_cReturn;
	GRFOVSIG		m_grfovsig;
	const char *	m_pChzDescription;
};

OverloadSignature s_aOvsig[] =
{
	{ PARK_AdditiveOp,			2, 1,	FOVSIG_AllowCommutative,	"(Lhs: A, Rhs: B)->C" },
	{ PARK_MultiplicativeOp,	2, 1,	FOVSIG_AllowCommutative,	"(Lhs: A, Rhs: B)->C" },
	{ PARK_ShiftOp,				2, 1,	false,	"(Lhs: A, Rhs: B)->C" },
	{ PARK_RelationalOp,		2, 1,	FOVSIG_ReturnBool,	"(Lhs: A, Rhs: B)->bool" },
	{ PARK_AssignmentOp,		2, 0,	FOVSIG_MustTakeReference,	"(Lhs: &B, Rhs: A)" },
	{ PARK_Decl,				2, 0,	FOVSIG_MustTakeReference,	"(Lhs: &B, Rhs: A)" },
	{ PARK_PostfixUnaryOp,		1, 1,	FOVSIG_MustTakeReference,	"(lHs: &A)->B" },
	{ PARK_UnaryOp,				1, 1,	false,	"(a: A)->B" },
};

const char * PChzOverloadSignature(PARK park)
{
	auto pOvsigMax = MOE_PMAC(s_aOvsig);
	for (OverloadSignature * pOvsig = s_aOvsig; pOvsig != pOvsigMax; ++pOvsig)
	{
		if (pOvsig->m_park == park)
			return pOvsig->m_pChzDescription;
	}
	return "Unknown";
}

bool FAllowsCommutative(PARK park)
{
	auto pOvsigMax = MOE_PMAC(s_aOvsig);
	for (OverloadSignature * pOvsig = s_aOvsig; pOvsig != pOvsigMax; ++pOvsig)
	{
		if (pOvsig->m_park == park)
			return pOvsig->m_grfovsig.FIsSet(FOVSIG_AllowCommutative);
	}
	return false;
}

const char * PChzOverloadNameFromTok(TOK tok)
{
	auto pOvinfMax = MOE_PMAC(s_aOvinf);
	for (auto pOvinf = s_aOvinf; pOvinf != pOvinfMax; ++pOvinf)
	{
		if (pOvinf->m_tok == tok)
		{
			return pOvinf->m_pChz;
		}
	}
	return nullptr;
}

static bool FOperatorOverloadMustTakeReference(TOK tok)
{
	auto pOvinfMax = MOE_PMAC(s_aOvinf);
	for (auto pOvinf = s_aOvinf; pOvinf != pOvinfMax; ++pOvinf)
	{
		if (pOvinf->m_tok != tok)
			continue;

		int acBool[2] = {0,0};
		auto pOvsigMax = MOE_PMAC(s_aOvsig);
		for (int iPark = 0; iPark < MOE_DIM(pOvinf->m_aPark); ++iPark)
		{
			for (OverloadSignature * pOvsig = s_aOvsig; pOvsig != pOvsigMax; ++pOvsig)
			{
				if (pOvsig->m_park == pOvinf->m_aPark[iPark])
				{
					++acBool[pOvsig->m_grfovsig.FIsSet(FOVSIG_MustTakeReference)];
				}
			}
			MOE_ASSERT(acBool[0] == 0 || acBool[1] == 0, "conflicting results for diferrent operators");
		}
		return acBool[true] != 0;
	}
	return false;
}

bool FCheckOverloadSignature(PARK park, TypeInfoProcedure * pTinproc)
{
	size_t cReturn = pTinproc->m_arypTinReturns.C();
	if (cReturn == 1 && pTinproc->m_arypTinReturns[0]->m_tink == TINK_Void)
	{
		cReturn = 0;
	}

	auto pOvsigMax = MOE_PMAC(s_aOvsig);
	for (OverloadSignature * pOvsig = s_aOvsig; pOvsig != pOvsigMax; ++pOvsig)
	{
		if (pOvsig->m_park == park)
		{
			if (pTinproc->m_arypTinParams.C() != pOvsig->m_cParam || cReturn != pOvsig->m_cReturn)
				return false;
			if (pOvsig->m_grfovsig.FIsSet(FOVSIG_ReturnBool) && pTinproc->m_arypTinReturns[0]->m_tink != TINK_Bool)
				return false;

			return !pOvsig->m_grfovsig.FIsSet(FOVSIG_MustTakeReference) || pTinproc->m_arypTinParams[0]->m_tink == TINK_Pointer;
		}
	}
	return false;
}

ERRID ErridCheckOverloadSignature(TOK tok, TypeInfoProcedure * pTinproc, ErrorManager * pErrman, const LexSpan & lexsp)
{
	auto pOvinfMax = MOE_PMAC(s_aOvinf);
	for (auto pOvinf = s_aOvinf; pOvinf != pOvinfMax; ++pOvinf)
	{
		if (pOvinf->m_tok != tok)
			continue;

		for (int iPark = 0; iPark < MOE_DIM(pOvinf->m_aPark); ++iPark)
		{
			PARK park = pOvinf->m_aPark[iPark];
			if (park != PARK_Nil && FCheckOverloadSignature(park, pTinproc))
			{
				if (pTinproc->m_grftinproc.FIsSet(FTINPROC_IsCommutative) && !FAllowsCommutative(park))
				{
					EmitError(pErrman, lexsp, ERRID_UnknownError, 
						"'%s' is not allowed when overloading '%s'", RWord::g_pChzCommutative, PChzFromTok(tok));
					return ERRID_UnknownError;
				}

				return ERRID_Nil;
			}
		}

		Error error(pErrman, ERRID_BadOverloadSig);
		PrintErrorLine(&error, "Error:", lexsp, "Incorrect signature for overloading operator '%s'. Options are:", PChzFromTok(tok));

		for (int iPark = 0; iPark < MOE_DIM(pOvinf->m_aPark); ++iPark)
		{
			PARK park = pOvinf->m_aPark[iPark];
			if (park != PARK_Nil)
			{
				PrintErrorLine(&error, "", lexsp, "\toperator%s%s'", PChzFromTok(tok), PChzOverloadSignature(park));
			}
		}
		return ERRID_BadOverloadSig;
	}

	EmitError(pErrman, lexsp, ERRID_UnknownError, "no supported overload signature for operator '%s'", PChzFromTok(tok));
	return ERRID_UnknownError;
}

STNode * PStnodParseReturnArrow(ParseContext * pParctx, Lexer * pLex, SymbolTable * pSymtabProc)
{
	if (FTryConsumeToken(pLex, TOK_Arrow))
	{
		// TODO : handle multiple return types
		ProcSymtabStack procss(pParctx);
		if (pSymtabProc)
		{
			LexSpan lexsp(pLex);
			pSymtabProc->m_iNestingDepth = pParctx->m_pSymtab->m_iNestingDepth + 1;
			procss.Push(pSymtabProc, lexsp);
			pParctx->m_pSymtabGeneric = pSymtabProc;
		}

		auto pStnodRet = PStnodParseTypeSpecifier(pParctx, pLex, "return value", FPDECL_AllowBakedTypes);
		if (pSymtabProc)
		{
			SymbolTable * pSymtabPop = procss.PSymtabPop();
			MOE_ASSERT(pSymtabProc == pSymtabPop, "CSymbol table push/pop mismatch (list)");
		}

		if (pStnodRet)
		{
			return pStnodRet;
		}

		EmitError(pParctx, LexSpan(pLex), ERRID_TypeSpecifierExpected, "expected type specification following return arrow.");
	}

	auto pStvalVoid = PStvalAllocateIdentifier(pParctx, pLex, LexSpan(pLex), BuiltIn::g_istrVoid);
	pStvalVoid->m_istr = BuiltIn::g_istrVoid;

	return pStvalVoid;
}

const char * PChzUnexpectedToken(Lexer * pLex)
{
	if (pLex->m_tok == TOK_Identifier)
	{
		return pLex->m_istr.m_pChz;
	}
	return PChzCurrentToken(pLex);
}

STNode * PStnodParseProcParameterList(ParseContext * pParctx, Lexer * pLex, SymbolTable * pSymtabProc, bool fIsOpOverload)
{
	LexSpan lexsp(pLex);
	ProcSymtabStack procss(pParctx);
	if (pSymtabProc)
	{
		pSymtabProc->m_iNestingDepth = pParctx->m_pSymtab->m_iNestingDepth + 1;
		procss.Push(pSymtabProc, lexsp);
		pParctx->m_pSymtabGeneric = pSymtabProc;
	}

	GRFPDECL grfpdecl = FPDECL_AllowVariadic | FPDECL_AllowBakedTypes | FPDECL_AllowBakedValues | FPDECL_AllowUsing | FPDECL_AllowUnnamed;
	bool fHasVarArgs = false;
	bool fNeedsDefaultArg = false;
	CDynAry<STNode *> arypStnodArg(pParctx->m_pAlloc, BK_Parse);

	while (pLex->m_tok != TOK(')'))
	{
		static const TOK s_aTok[] = {TOK(')'), TOK(','), TOK_EndOfLine};
		LexRecoverAmbit lrecamb(pParctx->m_pLrecst, s_aTok);

		auto pStnodParam = PStnodParseParameter(pParctx, pLex, pSymtabProc, grfpdecl);

		if (pStnodParam)
		{
			fHasVarArgs |= pStnodParam->m_park == PARK_VariadicArg;
			arypStnodArg.Append(pStnodParam);

			auto pStdecl = PStnodRtiCast<STDecl *>(pStnodParam);
			if (pStnodParam->m_park == PARK_Decl && MOE_FVERIFY(pStdecl, "expected parameter to be PARK_Decl"))
			{
				bool fHasDefaultArg = pStdecl->m_pStnodInit;
				if (fNeedsDefaultArg && !fHasDefaultArg)
				{
					InString istrIdent(IstrFromIdentifier(pStdecl->m_pStnodIdentifier));
					EmitError(pParctx, LexSpan(pLex), ERRID_MissingDefaultArgs, 
						"parameter '%s' must have a default argument because earlier arguments have defaults.", istrIdent.m_pChz);
				}
				if (fHasDefaultArg && fIsOpOverload)
				{
					InString istrIdent(IstrFromIdentifier(pStdecl->m_pStnodIdentifier));
					EmitError(pParctx, LexSpan(pLex), ERRID_DefaultParamOpOverload, 
						"default values for parameter '%s' is not allowed on an operator overload", istrIdent.m_pChz);
				}

				fNeedsDefaultArg |= fHasDefaultArg;
			}
		}
		else
		{
			auto pChzUnexpected = PChzUnexpectedToken(pLex);
			EmitError(pParctx, LexSpan(pLex), ERRID_ExpectedParameter, "expected parameter declaration before '%s'", pChzUnexpected);
			(void) TokSkipToRecovery(pLex, pParctx->m_pLrecst);	
		}

		if (pLex->m_tok != TOK(')') && !FTryConsumeToken(pLex, TOK(',')))
		{
			auto strUnexpected = StrUnexpectedToken(pLex);
			EmitError(pParctx, LexSpan(pLex), ERRID_MissingToken, "Expected ',' before '%s'", strUnexpected.m_pChz);

			(void) TokSkipToRecovery(pLex, pParctx->m_pLrecst);	

			if (!FTryConsumeToken(pLex, TOK(',')))
				break;
		}
	}

	STNode * pStnodList = nullptr;
	if (!arypStnodArg.FIsEmpty())
	{
		pStnodList = PStnodAlloc<STNode>(pParctx->m_pAlloc, PARK_ParameterList, pLex, LexSpan(pLex));
		pStnodList->m_pSymtab = pSymtabProc;
		pStnodList->CopyChildArray(pParctx->m_pAlloc, arypStnodArg.A(), int(arypStnodArg.C()));
	}

	if (fHasVarArgs)
	{
		STNode * pStnodLast = pStnodList->PStnodChild(int(pStnodList->m_cpStnodChild)-1);
		if (pStnodLast->m_park != PARK_VariadicArg)
		{
			EmitError(pParctx, LexSpan(pLex), ERRID_VariadicMustBeLast, 
				"Variadic function argument found before the end of the argument list");
		}
	}

	if (pSymtabProc)
	{
		SymbolTable * pSymtabPop = procss.PSymtabPop();
		MOE_ASSERT(pSymtabProc == pSymtabPop, "CSymbol table push/pop mismatch (list)");
	}

	return pStnodList;
}

STNode ** PPStnodChildFromPark(STNode * pStnod, int * pcpStnodChild, PARK park)
{
	if (!pStnod || pStnod->m_park != park)
	{
		*pcpStnodChild = 0;
		return nullptr;
	}

	*pcpStnodChild = pStnod->m_cpStnodChild;
	return pStnod->m_apStnodChild;
}

void CheckGenericParams(ParseContext * pParctx, STNode * pStnodRoot, CHash<InString, STNode *> * pmpIstrPStnod, GRFTINGEN * pGrftingen)
{
	CDynAry<STNode *> arypStnodStack(pParctx->m_pAlloc, BK_Parse);

	arypStnodStack.Append(pStnodRoot);
	while (arypStnodStack.C())
	{
		auto pStnodIt = arypStnodStack.TPopLast();
		if (pStnodIt->m_park == PARK_GenericDecl)
		{
			auto pTinanc = PTinDerivedCast<TypeInfoAnchor *>(pStnodIt->m_pTin);
			if (!pTinanc)
				continue;

			pGrftingen->AddFlags(FTINGEN_HasBakedTypeArgs);

			STNode ** ppStnodHash;
			INRES inres = pmpIstrPStnod->InresEnsureKey(pTinanc->m_istrName, &ppStnodHash);
			if (inres == INRES_AlreadyExisted)
			{
				LexLookup lexlook(pParctx->m_pWork, (*ppStnodHash)->m_lexsp);

				EmitError(pParctx->PErrman(), pStnodIt->m_lexsp, ERRID_MultipleAnchorDef, 
					"Generic type $%s is was already anchored here %s(%d,%d)",
					pTinanc->m_istrName.m_pChz,
					lexlook.m_istrFilename.m_pChz, lexlook.m_iLine, lexlook.m_iCodepoint);
			}
			else
			{
				*ppStnodHash = pStnodIt;
			}

			continue;
		}
		else if (pStnodIt->m_park == PARK_Decl)
		{
			auto pStdecl = PStnodDerivedCast<STDecl *>(pStnodIt);

			// NOTE: if we have a baked type that is a parameter in another type, we're dealing with 
			//  a generic type - not a baked value
			if (pStdecl->m_fIsBakedConstant)
			{
				pGrftingen->AddFlags(FTINGEN_HasBakedTypeArgs);
			}
		}

		int cpStnodChild = int(pStnodIt->m_cpStnodChild);
		for (int ipStnodChild = 0; ipStnodChild < cpStnodChild; ++ipStnodChild)
		{
			arypStnodStack.Append(pStnodIt->PStnodChild(ipStnodChild));
		}
	}
}

STNode * PStnodFindChildPark(ParseContext * pParctx, STNode * pStnodRoot, PARK park)
{
	CDynAry<STNode *> arypStnodStack(pParctx->m_pAlloc, BK_Parse);

	arypStnodStack.Append(pStnodRoot);
	while (arypStnodStack.C())
	{
		auto pStnodIt = arypStnodStack.TPopLast();
		if (pStnodIt->m_park == park)
			return pStnodIt;

		int cpStnodChild = pStnodIt->CPStnodChild();
		for (int ipStnodChild = 0; ipStnodChild < cpStnodChild; ++ipStnodChild)
		{
			arypStnodStack.Append(pStnodIt->PStnodChild(ipStnodChild));
		}
	}
	return nullptr;
}

void CheckTinprocGenerics(ParseContext * pParctx, STNode * pStnodProc, TypeInfoProcedure * pTinproc)
{
	auto pStproc = PStnodDerivedCast<STProc *>(pStnodProc);
	if (!pStproc)
		return;

	auto pStnodParameterList = pStproc->m_pStnodParameterList;
	if (pStnodParameterList)
	{
		CHash<InString, STNode *> mpIstrPStnod(pParctx->m_pAlloc, BK_Parse);

		int cpStnodParam = pStnodParameterList->m_cpStnodChild;
		for (int ipStnodParam = 0; ipStnodParam < cpStnodParam; ++ipStnodParam)
		{
			auto pStnodParam = pStnodParameterList->PStnodChild(ipStnodParam);
			auto pStdecl = PStnodRtiCast<STDecl *>(pStnodParam);
			if (!pStdecl)
				continue;

			auto pStnodType = pStdecl->m_pStnodType;
			if (pStdecl->m_fIsBakedConstant)
			{
				pTinproc->m_grftingen.AddFlags(FTINGEN_HasBakedValueArgs);
			}

			pTinproc->m_mpIptinGrfparmq[ipStnodParam].AssignFlags(FPARMQ_BakedValue, pStdecl->m_fIsBakedConstant);
			pTinproc->m_mpIptinGrfparmq[ipStnodParam].AssignFlags(FPARMQ_TypeArgument, pStdecl->m_pStnodIdentifier == nullptr);

			if (pStnodType)
			{
				CheckGenericParams(pParctx, pStnodType, &mpIstrPStnod, &pTinproc->m_grftingen);
			}
		}
	}

	auto pStnodReturn = pStproc->m_pStnodReturnType;
	if (pStnodReturn)
	{
		auto pStnodGeneric = PStnodFindChildPark(pParctx, pStnodReturn, PARK_GenericDecl);
		if (pStnodGeneric)
		{
			auto pTinanc = PTinDerivedCast<TypeInfoAnchor *>(pStnodGeneric->m_pTin);

			EmitError(pParctx->m_pWork->m_pErrman, pStnodReturn->m_lexsp, ERRID_NoGenericReturn,
				"Generic type anchor '$%s' is not allowed in return type",
				(pTinanc) ? pTinanc->m_istrName.m_pChz : "unknown");
		}
	}
}

void CheckTinstructGenerics(ParseContext * pParctx, STNode * pStnodStruct, TypeInfoStruct * pTinstruct)
{
	auto pStstruct = PStnodDerivedCast<STStruct *>(pStnodStruct);
	if (!pStstruct)
		return;

#if KEEP_TYPEINFO_DEBUG_STRING
	pTinstruct->m_strDebug = StrFromTypeInfo(pTinstruct);
#endif

	auto pStnodParameterList = pStstruct->m_pStnodParameterList;
	if (pStnodParameterList)
	{
		CHash<InString, STNode *> mpIstrPStnod(pParctx->m_pAlloc, BK_Parse);

		int cpStnodParam = pStnodParameterList->CPStnodChild();
		for (int ipStnodParam = 0; ipStnodParam < cpStnodParam; ++ipStnodParam)
		{
			auto pStnodParam = pStnodParameterList->PStnodChild(ipStnodParam);
			auto pStdecl = PStnodRtiCast<STDecl *>(pStnodParam);
			if (!pStdecl)
				continue;

			auto pStnodType = pStdecl->m_pStnodType;
			if (pStdecl->m_fIsBakedConstant)
			{
				pTinstruct->m_grftingen.AddFlags(FTINGEN_HasBakedValueArgs);
			}

			if (pStnodType)
			{
				CheckGenericParams(pParctx, pStnodType, &mpIstrPStnod, &pTinstruct->m_grftingen);
			}
		}
	}
}

STNode * PStnodSpoofEnumConstant(ParseContext * pParctx, Lexer * pLex, const LexSpan & lexsp, const InString & istrIdent, PARK park)
{
	auto pStdeclConstant = PStnodAlloc<STDecl>(pParctx->m_pAlloc, park, pLex, lexsp);
	pStdeclConstant->m_grfstnod.AddFlags(FSTNOD_ImplicitMember);

	auto pStvalIdent = PStvalAllocateIdentifier(pParctx, pLex, lexsp, istrIdent);
	pStvalIdent->m_grfstnod.AddFlags(FSTNOD_ImplicitMember);
	pStdeclConstant->m_pStnodIdentifier = pStvalIdent;

	pStdeclConstant->m_pSymbase = pParctx->m_pSymtab->PSymEnsure(pParctx->m_pWork->m_pErrman, istrIdent, pStdeclConstant, FSYM_VisibleWhenNested);
	return pStdeclConstant;
}

STNode * PStnodParseEnumConstant(ParseContext * pParctx, Lexer * pLex)
{
	LexSpan lexsp(pLex);
	auto pStvalIdent = PStvalParseIdentifier(pParctx, pLex);
	if (!pStvalIdent)
		return nullptr;

	auto pStdeclConstant = PStnodAlloc<STDecl>(pParctx->m_pAlloc, PARK_EnumConstant, pLex, lexsp);
	pStdeclConstant->m_pStnodIdentifier = pStvalIdent;

	SymbolTable * pSymtab = pParctx->m_pSymtab;
	InString istrIdent = IstrFromIdentifier(pStvalIdent);

	// BB - we should change the interface here so we don't do multiple lookups
	auto pErrman = pParctx->m_pWork->m_pErrman;
	auto pSym = pSymtab->PSymLookup(istrIdent, pStdeclConstant->m_lexsp);
	if (pSym)
	{
		LexLookup lexlook(pErrman->m_pWork, pSym->m_pStnodDefinition->m_lexsp);
		EmitError(pErrman, pStdeclConstant->m_lexsp, ERRID_EnumRepeat, 
			"Enum constant name '%s' has already been defined at %s(%d, %d)", 
			istrIdent.m_pChz, 
			lexlook.m_istrFilename.m_pChz, lexlook.m_iLine, lexlook.m_iCodepoint);
	}

	pSym = pSymtab->PSymEnsure(pErrman, istrIdent, pStdeclConstant, FSYM_VisibleWhenNested);
	pStdeclConstant->m_pSymbase = pSym;

	if (FTryConsumeToken(pLex, TOK_ColonEqual))
	{
		STNode * pStnodExp = PStnodParseExpression(pParctx, pLex);
		if (pStnodExp)
		{
			pStdeclConstant->m_pStnodInit = pStnodExp;
		}
	}
	return pStdeclConstant;
}

STNode * PStnodParseEnumConstantList(ParseContext * pParctx, Lexer * pLex, STEnum * pStenum)
{
	if (!FExpectConsumeToken(pParctx, pLex, TOK('{')))
	{
		(void) TokSkipToRecovery(pLex, pParctx->m_pLrecst);	
		return nullptr;
	}

	LexSpan lexsp(pLex);
	auto pStnodList = PStnodAlloc<STNode>(pParctx->m_pAlloc, PARK_List, pLex, lexsp);
	pStnodList->m_tok = TOK('{');

	CDynAry<STNode *> arypStnod(pParctx->m_pAlloc, BK_Parse);

	// add STNodes for implicit members: nil, min, max, etc.

	for (int enumimp = ENUMIMP_Min; enumimp < ENUMIMP_Max; ++enumimp)
	{
		if (!FNeedsImplicitMember((ENUMIMP)enumimp, pStenum->m_enumk))
			continue;

		PARK park = ((enumimp == ENUMIMP_Names) | (enumimp == ENUMIMP_Values)) ? PARK_CompoundLiteral : PARK_EnumConstant;
		auto pStnodImplicit = PStnodSpoofEnumConstant(pParctx, pLex, lexsp, IstrFromEnumimp((ENUMIMP)enumimp), park);

		pStenum->m_mpEnumimpIstnod[enumimp] = int(arypStnod.C());
		arypStnod.Append(pStnodImplicit);
	
		if (park == PARK_EnumConstant)
		{
			++pStenum->m_cConstantImplicit;
		}
	}

	static const TOK s_aTok[] = {TOK(','), TOK('}') };
	LexRecoverAmbit lrecamb(pParctx->m_pLrecst, s_aTok);

	while (pLex->m_tok != TOK('}'))
	{
		STNode * pStnod = PStnodParseEnumConstant(pParctx, pLex);
		if (pStnod)
		{
			arypStnod.Append(pStnod);
			++pStenum->m_cConstantExplicit;
		}
		else
		{
			auto strUnexpected = StrUnexpectedToken(pLex);
			EmitError(pParctx, LexSpan(pLex), ERRID_EnumConstantExpected, "Expected enum constant but found '%s'", strUnexpected.m_pChz);
			(void) TokSkipToRecovery(pLex, pParctx->m_pLrecst);	
		}

		if (pLex->m_tok != TOK('}'))
		{
			if (!FTryConsumeToken(pLex, TOK(',')))
			{
				auto strUnexpected = StrUnexpectedToken(pLex);
				EmitError(pParctx, LexSpan(pLex), ERRID_MissingToken, "Expected ',' but found '%s'", strUnexpected.m_pChz);
				(void) TokSkipToRecovery(pLex, pParctx->m_pLrecst);	

				if (!FTryConsumeToken(pLex, TOK(',')))
					break;
			}
		}
	}

	if (!FExpectConsumeToken(pParctx, pLex, TOK('}')))
	{
		(void) TokSkipToRecovery(pLex, pParctx->m_pLrecst);
	}

	pStnodList->CopyChildArray(pParctx->m_pAlloc, arypStnod.A(), int(arypStnod.C()));
	return pStnodList;
}

bool FTrySkipContinuation(ParseContext * pParctx, Lexer * pLex, TOK tokContinue, TOK tokNext)
{
	if (pLex->m_tok == tokNext)
		return true;

	if (!FTryConsumeToken(pLex, tokContinue))
	{
		auto strUnexpected = StrUnexpectedToken(pLex);
		EmitError(pParctx, LexSpan(pLex), ERRID_MissingToken, 
			"Expected '%s' but found '%s'", PChzFromTok(tokContinue), strUnexpected.m_pChz);
		(void) TokSkipToRecovery(pLex, pParctx->m_pLrecst);	

		if (!FTryConsumeToken(pLex, TOK(',')))
			return false;
	}
	return true;	
}

STNode * PStnodParseMemberDeclList(ParseContext * pParctx, Lexer * pLex)
{
	static const TOK s_aTok[] = {TOK('}'), TOK(';'), TOK_EndOfLine };
	LexRecoverAmbit lrecamb(pParctx->m_pLrecst, s_aTok);

	STNode * pStnodList = nullptr;
	CDynAry<STNode *> arypStnod(pParctx->m_pAlloc, BK_Parse);

	LexSpan lexsp(pLex);
	while (pLex->m_tok != TOK('}'))
	{

		STNode * pStnod = PStnodParseDecl(pParctx, pLex);
		if (!pStnod)
		{
			pStnod = PStnodParseDefinition(pParctx, pLex);
		}

		if (!pStnod && FTryConsumeToken(pLex, TOK(';')))
		{
			pStnod = PStnodAlloc<STNode>(pParctx->m_pAlloc, PARK_Nop, pLex, LexSpan(pLex));
		}

		if (pStnod)
		{
			arypStnod.Append(pStnod);
		}
		else
		{
			auto strUnexpected = StrUnexpectedToken(pLex);
			EmitError(pParctx, LexSpan(pLex), ERRID_UnexpectedToken, 
				"Expected member declaration or definition but found '%s'", strUnexpected.m_pChz);
			(void) TokSkipToRecovery(pLex, pParctx->m_pLrecst);	
		}
	}

	if (!arypStnod.FIsEmpty())
	{
		pStnodList = PStnodAlloc<STNode>(pParctx->m_pAlloc, PARK_List, pLex, lexsp);
		pStnodList->m_tok = TOK('{');
		pStnodList->CopyChildArray(pParctx->m_pAlloc, arypStnod.A(), int(arypStnod.C()));
	}

	return pStnodList;
}


STNode * PStnodParseDefinition(ParseContext * pParctx, Lexer * pLex)
{
	if (pLex->m_tok == TOK_Identifier)
	{
		InString istrRword;
		bool fIsDefinition = false;
		if (pLex->m_istr == RWord::g_istrOperator)
		{
			fIsDefinition = true;
			istrRword = pLex->m_istr;
		}

		Lexer lexPeek = *pLex;
		TokNext(&lexPeek);

		if (istrRword.FIsNull())
		{
			istrRword = lexPeek.m_istr;

			if (istrRword == RWord::g_istrProc || 
				istrRword == RWord::g_istrStruct ||
				istrRword == RWord::g_istrEnum ||
				istrRword == RWord::g_istrFlagEnum ||
				istrRword == RWord::g_istrTypedef)
			{
				fIsDefinition = true;
			}
		}

		if (!fIsDefinition)
			return nullptr;

		LexSpan lexsp(pLex);
		STNode * pStnodIdent;

		if (istrRword == RWord::g_istrOperator)
		{
			TokNext(pLex);

			const char * pChzOverloadName = PChzOverloadNameFromTok((TOK)pLex->m_tok);
			if (!pChzOverloadName)
			{
				EmitError(pParctx, lexsp, ERRID_InvalidOpOverload, "Cannot overload operator '%s'", PChzFromTok((TOK)pLex->m_tok));
				pChzOverloadName = "OverloadError";
			}

			Moe::InString istrIdent = IstrIntern(pChzOverloadName);
			pStnodIdent = PStvalAllocateIdentifier(pParctx, pLex, lexsp, istrIdent);
			pStnodIdent->m_tok = TOK(pLex->m_tok);
		}
		else
		{
			pStnodIdent = PStvalParseIdentifier(pParctx, pLex);
			pStnodIdent->m_tok = TOK_Identifier;
		}

		*pLex = lexPeek;
		TokNext(pLex);
		
		// function definition
		if (istrRword == RWord::g_istrProc || istrRword == RWord::g_istrOperator)
		{
			FExpectConsumeToken(pParctx, pLex, TOK('('));

			auto pStproc = PStnodAlloc<STProc>(pParctx->m_pAlloc, PARK_ProcedureDefinition, pLex, LexSpan(pLex));
			pStproc->m_grfstnod.AddFlags(FSTNOD_EntryPoint);

			pStproc->m_pStnodName = pStnodIdent;
			pStproc->m_pStnodParentScope = pParctx->m_pStnodScope;

			Moe::InString istrName = IstrFromIdentifier(pStnodIdent);
			SymbolTable * pSymtabParent = pParctx->m_pSymtab;
			SymbolTable * pSymtabProc = PSymtabNew(pParctx->m_pAlloc, pSymtabParent, istrName);

			// BB - don't mangle the main function so the linker can find it. yuck.
			if (istrName == RWord::g_istrMain)
			{
				pStproc->m_grfstproc.AddFlags(FSTPROC_UseUnmangledName);
			}

			STNode * pStnodParams = PStnodParseProcParameterList(pParctx, pLex, pSymtabProc, istrRword == RWord::g_istrOperator);
			pStproc->m_pStnodParameterList = pStnodParams;
			FExpectConsumeToken(pParctx, pLex, TOK(')'));

			auto pStnodReturns = PStnodParseReturnArrow(pParctx, pLex, pSymtabProc);
			pStproc->m_pStnodReturnType = pStnodReturns;

			INLINEK inlinek = INLINEK_Nil;
			MCALLCON callconv = MCALLCON_Nil;
			GRFTINPROC grftinproc;
			pStproc->m_pStnodBody = nullptr;
			if (pLex->m_tok == TOK_Identifier)
			{
				while (pLex->m_tok == TOK_Identifier)
				{
					InString istrRwordLookup = pLex->m_istr;
					TokNext(pLex);

					if (istrRwordLookup == RWord::g_istrForeignDirective)
					{
						pStproc->m_grfstproc.AddFlags(FSTPROC_UseUnmangledName);
						grftinproc.AddFlags(FTINPROC_IsForeign);

						if (!FIsEndOfStatement(pLex) && pLex->m_tok == TOK_Identifier)
						{
							pStproc->m_pStnodForeignAlias = PStvalParseIdentifier(pParctx, pLex);
						}
					}
					else if (istrRwordLookup == RWord::g_istrCDecl)
					{
						callconv = MCALLCON_CX86;
						pStproc->m_grfstproc.AddFlags(FSTPROC_UseUnmangledName | FSTPROC_PublicLinkage);
					}
					else if (istrRwordLookup == RWord::g_istrStdCall)
					{
						callconv = MCALLCON_StdcallX86;	
					}
					else if (istrRwordLookup == RWord::g_istrInline)
					{
						inlinek = INLINEK_AlwaysInline;
					}
					else if (istrRwordLookup == RWord::g_istrNoInline)
					{
						inlinek = INLINEK_NoInline;
					}
					else if (istrRwordLookup == RWord::g_istrCommutative)
					{
						if (istrRword == RWord::g_istrOperator)
						{
							grftinproc.AddFlags(FTINPROC_IsCommutative);
						}
						else
						{
							EmitError(pParctx, LexSpan(pLex), ERRID_OnlyOperatorCanCommute,
								"only operator overloads can be declared commutative, %s() is not an overload.\n", istrName.m_pChz);
						}
					}
					else
					{
						EmitError(pParctx, LexSpan(pLex), ERRID_UnexpectedToken,
							"Unexpected token following procedure declaration %s\n", istrRwordLookup);
					}
				}

				if (pLex->m_tok != TOK('{'))
				{
					ExpectEndOfStatement(pParctx, pLex, "While parsing procedure qualifiers");
				}
			}

			LexSpan lexspBody(pLex);
			if (pLex->m_tok == TOK('{'))
			{
				auto pStnodScopePrev = pParctx->m_pStnodScope;
				pParctx->m_pStnodScope = pStproc;
				STNode * pStnodBody = PStnodParseCompoundStatement(pParctx, pLex, pSymtabProc);
				pParctx->m_pStnodScope = pStnodScopePrev;

				pStproc->m_pStnodBody = pStnodBody;
			}

			if (grftinproc.FIsSet(FTINPROC_IsForeign))
			{
				if (pStproc->m_pStnodBody != nullptr)
				{
					EmitError(pParctx, lexspBody, ERRID_ForeignProcDefinesBody,
						"Procedure '%s' is marked foreign, but defines a procedure body.", istrName.m_pChz);
					pStproc->m_pStnodBody = nullptr;
				}
			}
			else if (pStproc->m_pStnodBody == nullptr)
			{
				EmitError(pParctx, lexspBody, ERRID_ProcBodyExpected, "Procedure definition for '%s' has no body", istrName.m_pChz);
			}

			int cStnodParams;
			STNode ** ppStnodParams = PPStnodChildFromPark(pStnodParams, &cStnodParams, PARK_ParameterList);

			STNode ** ppStnodReturn = &pStnodReturns;
			int cStnodReturns = (pStnodReturns == nullptr) ? 0 : 1;

			auto pTinproc = pSymtabParent->PTinprocAllocate(istrName, cStnodParams, cStnodReturns);

			if (istrRword == RWord::g_istrOperator && FOperatorOverloadMustTakeReference(pStnodIdent->m_tok))
			{
				pTinproc->m_mpIptinGrfparmq[0].AddFlags(FPARMQ_ImplicitRef);
			}

			if (grftinproc.FIsSet(FTINPROC_IsCommutative))
			{
				if (cStnodParams != 2)
				{
					EmitError(pParctx, pStproc->m_lexsp, ERRID_OnlyTwoArgsCommute,
						"Only operators with two arguments can be commutative ('%s' has %d)", istrName.m_pChz, cStnodParams);
					grftinproc.Clear(FTINPROC_IsCommutative);
				}
			}

			pTinproc->m_grftinproc = grftinproc;
			pTinproc->m_pStnodDefinition = pStproc;
			pTinproc->m_callconv = callconv;
			pTinproc->m_inlinek = inlinek;

			CheckTinprocGenerics(pParctx, pStproc, pTinproc);

			STNode ** ppStnodParamMax = &ppStnodParams[cStnodParams];
			for ( ; ppStnodParams != ppStnodParamMax; ++ppStnodParams)
			{
				STNode * pStnodParam = *ppStnodParams;
				if (pStnodParam->m_park == PARK_VariadicArg)
				{
					pTinproc->m_grftinproc.AddFlags(FTINPROC_HasVarArgs);
				}
				else if (AST_FVERIFY(pParctx->m_pWork, pStnodParam, pStnodParam->m_park == PARK_Decl, "Expected decl"))
				{
					pTinproc->m_arypTinParams.Append(pStnodParam->m_pTin);
				}
			}

			STNode ** ppStnodReturnMax = &ppStnodReturn[cStnodReturns];
			for ( ; ppStnodReturn != ppStnodReturnMax; ++ppStnodReturn)
			{
				pTinproc->m_arypTinReturns.Append((*ppStnodReturn)->m_pTin);
			}

			pStproc->m_pTin = pTinproc;

			if (pStproc->m_pStnodBody != nullptr)
			{
				bool fReturnsVoid = false;
				if (MOE_FVERIFY(pStproc->m_pStnodReturnType != nullptr, "return type expected. implicit void should be set by here"))
				{
					STNode * pStnodReturn = pStproc->m_pStnodReturnType;
					fReturnsVoid = FIsIdentifier(pStnodReturn, BuiltIn::g_istrVoid);
				}

				STNode * pStnodBody = pStproc->m_pStnodBody;

				if (MOE_FVERIFY( pStnodBody->m_park == PARK_List, "Expected body list"))
				{
					STNode * pStnodLast = pStnodBody->PStnodChildSafe(pStnodBody->CPStnodChild()-1);

					bool fHasReturn = pStnodLast && FIsIdentifier(pStnodLast, RWord::g_istrReturn);
					if (!fHasReturn)
					{
						if (fReturnsVoid)
						{
							LexSpan lexsp(pLex);
							auto pStvalReturn = PStnodAlloc<STValue>(pParctx->m_pAlloc, PARK_ReservedWord, pLex, LexSpan(pLex));
							pStvalReturn->m_tok = TOK_Identifier;
							pStvalReturn->m_istr = RWord::g_istrReturn;

							(void) pStnodBody->AppendChildToArray(pParctx->m_pAlloc, pStvalReturn);
						}
						else
						{
							EmitError(pParctx, LexSpan(pLex), ERRID_NoReturnStatement, 
								"Procedure '%s' is missing return statement", istrName.m_pChz);
						}
					}
				}
			}

			auto pErrman = pParctx->m_pWork->m_pErrman;
			Symbol * pSymProc = pSymtabParent->PSymEnsure(pErrman, IstrFromIdentifier(pStnodIdent), pStproc, FSYM_VisibleWhenNested);
			//pSymProc->m_pTin = pTinproc;
			pStproc->m_pSymbase = pSymProc;

			return pStproc;
		}
		else if (istrRword == RWord::g_istrEnum || istrRword == RWord::g_istrFlagEnum)
		{
			LexSpan lexsp(pLex);
			auto pStenum = PStnodAlloc<STEnum>(pParctx->m_pAlloc, PARK_EnumDefinition, pLex, LexSpan(pLex));

			pStenum->m_pStnodIdentifier = pStnodIdent;
			pStenum->m_enumk = (istrRword == RWord::g_istrFlagEnum) ? ENUMK_FlagEnum : ENUMK_Basic;

			STNode * pStnodType = PStnodParseTypeSpecifier(pParctx, pLex, "looseType", FPDECL_None);
			pStenum->m_pStnodType = pStnodType;
			
			InString istrIdent = IstrFromIdentifier(pStnodIdent);
			SymbolTable * pSymtabParent = pParctx->m_pSymtab;
			SymbolTable * pSymtabEnum = PSymtabNew(pParctx->m_pAlloc, pSymtabParent, istrIdent);
			STNode * pStnodConstantList = nullptr;

			auto pErrman = pParctx->m_pWork->m_pErrman;
			(void) pSymtabEnum->PSymEnsure(pErrman, IstrIntern("loose"), pStenum, FSYM_IsType | FSYM_VisibleWhenNested);
			(void) pSymtabEnum->PSymEnsure(pErrman, IstrIntern("strict"), pStenum, FSYM_IsType | FSYM_VisibleWhenNested);

			pSymtabEnum->m_iNestingDepth = pParctx->m_pSymtab->m_iNestingDepth + 1;
			PushSymbolTable(pParctx, pSymtabEnum);
			pStenum->m_pSymtab = pSymtabEnum;

			pStnodConstantList = PStnodParseEnumConstantList(pParctx, pLex, pStenum);
			pStenum->m_pStnodConstantList = pStnodConstantList;

			SymbolTable * pSymtabPop = PSymtabPop(pParctx);
			MOE_ASSERT(pSymtabEnum == pSymtabPop, "CSymbol table push/pop mismatch (enum)");

			// type info enum
			int cConstant = pStenum->m_cConstantImplicit + pStenum->m_cConstantExplicit;
			auto pTinenum = pSymtabParent->PTinenumAllocate(istrIdent, cConstant, pStenum->m_enumk, pStenum);

			int cpStnodChild = (pStnodConstantList) ? pStnodConstantList->CPStnodChild() : 0;
			for (int ipStnod = 0; ipStnod < cpStnodChild; ++ipStnod) 
			{
				STNode * pStnodMember = pStnodConstantList->PStnodChild(ipStnod);

				switch (pStnodMember->m_park)
				{
				case PARK_EnumConstant:
					{
						auto pTinlit = MOE_NEW(pSymtabParent->m_pAlloc, TypeInfoLiteral) TypeInfoLiteral();
						pSymtabParent->AddManagedTin(pTinlit);
						pTinlit->m_litty.m_litk = LITK_Enum;
						pTinlit->m_pTinSource = pTinenum;
						pStnodMember->m_pTin = pTinlit;
					} break;
				case PARK_CompoundLiteral:
					{
					} break;
				default: MOE_ASSERT(false, "Expected enum child value");
				}
			}

			GRFSYM grfsym(FSYM_IsType | FSYM_VisibleWhenNested);
			Symbol * pSymEnum = pSymtabParent->PSymEnsure(pErrman, istrIdent, pStenum, grfsym, FSHADOW_NoShadowing);
			//pSymEnum->m_pTin = pTinenum;

			pStenum->m_pSymbase = pSymEnum;
			pStenum->m_pTin = pTinenum;

			return pStenum;
		}
		else if (istrRword == RWord::g_istrStruct)
		{
			auto pStstruct = PStnodAlloc<STStruct>(pParctx->m_pAlloc, PARK_StructDefinition, pLex, LexSpan(pLex));
			pStstruct->m_pStnodIdentifier = pStnodIdent;

			SymbolTable * pSymtabParent = pParctx->m_pSymtab;

			InString istrIdent = IstrFromIdentifier(pStnodIdent);
			SymbolTable * pSymtabStruct = PSymtabNew(pParctx->m_pAlloc, pSymtabParent, istrIdent);

			if (FTryConsumeToken(pLex, TOK('(')))
			{
				STNode * pStnodParams = PStnodParseProcParameterList(pParctx, pLex, pSymtabStruct, false);

				pStstruct->m_pStnodParameterList = pStnodParams;
				FExpectConsumeToken(pParctx, pLex, TOK(')'));

				if (!pStnodParams)
				{
					EmitError(pParctx, LexSpan(pLex), ERRID_StructureParamsExpected, "Structure definition has parameter list, but no parameters.");
				}
				else if (MOE_FVERIFY(pStnodParams->m_park == PARK_ParameterList, "expected parameter list"))
				{
					for (int ipStnodParam = 0; ipStnodParam < pStnodParams->CPStnodChild(); ++ipStnodParam)
					{
						auto pStnodParam = pStnodParams->PStnodChild(ipStnodParam);
						switch (pStnodParam->m_park)
						{
						case PARK_Decl:
							{
								auto pStdecl = PStnodDerivedCast<STDecl*>(pStnodParam);
								auto strIdent = IstrFromIdentifier(pStdecl->m_pStnodIdentifier);
								if (!pStdecl->m_fIsBakedConstant && pStdecl->m_pStnodIdentifier)
								{
									// no need for named instances for values that won't be passed
									EmitError(pParctx, pStnodParam->m_lexsp, ERRID_NonBakedStructParameter,
										"Structure argument '%s' is neither be baked value or unnamed type argument", 
										istrIdent.m_pChz);
								}
							} break;
						default: 
							EmitError(pParctx, pStnodParam->m_lexsp, ERRID_BadStructGenericParam, 
								"unexpected generic struct parameter kind (%s)", PChzLongFromPark(pStnodParam->m_park));
						}
					}

				}
			}

			FExpectConsumeToken(pParctx, pLex, TOK('{'));

			// NOTE: struct symbol tables at the global scope should be unordered.
//				if (!pParctx->m_pSymtab->m_grfsymtab.FIsSet(FSYMTAB_Ordered))
//					pSymtabStruct->m_grfsymtab.Clear(FSYMTAB_Ordered);

			pSymtabStruct->m_iNestingDepth = pParctx->m_pSymtab->m_iNestingDepth + 1;
			PushSymbolTable(pParctx, pSymtabStruct);
			pStstruct->m_pSymtab = pSymtabStruct;

			STNode * pStnodDeclList = PStnodParseMemberDeclList(pParctx, pLex);

			SymbolTable * pSymtabPop = PSymtabPop(pParctx);
			MOE_ASSERT(pSymtabStruct == pSymtabPop, "CSymbol table push/pop mismatch (struct)");

			if (pStnodDeclList)
			{
				pStstruct->m_pStnodDeclList = pStnodDeclList;
			}
			else
			{
				EmitError(pParctx, LexSpan(pLex), ERRID_EmptyStruct, 
					"structure '%s' has no members - zero byte structures are not allowed", istrIdent.m_pChz);
			}

			// type info struct
			int cpStnodMember;
			STNode ** ppStnodMember = PPStnodChildFromPark(pStnodDeclList, &cpStnodMember, PARK_List);

			int cStnodField = 0;
			STNode * const * ppStnodMemberMax = &ppStnodMember[cpStnodMember];
			for (auto ppStnodMemberIt = ppStnodMember; ppStnodMemberIt != ppStnodMemberMax; ++ppStnodMemberIt)
			{
				auto pStnodMemberIt = *ppStnodMemberIt;
				switch (pStnodMemberIt->m_park)
				{
				case PARK_Decl:			
					{
						auto pStdecl = PStnodRtiCast<STDecl *>(pStnodMemberIt);
						if (MOE_FVERIFY(pStdecl, "expected stdecl"))
						{
#ifdef MOEB_LATER
							// fixme when we support compound decls again
							if (pStdecl->m_iStnodChildMin == -1)	
							{
								++cStnodField; 
							}
							else
							{
								cStnodField += pStdecl->m_iStnodChildMax - pStdecl->m_iStnodChildMin;
							}
#else
							++cStnodField;
#endif
						}
					} break;
				case PARK_ConstantDecl:		break;
				case PARK_StructDefinition:	break;
				case PARK_EnumDefinition:	break;
				case PARK_Typedef:			break;
				case PARK_Nop:				break;
				default: MOE_ASSERT(false, "Unexpected member in structure %s", istrIdent.m_pChz);
				}
			}

			STNode * pStnodParameterList = pStstruct->m_pStnodParameterList;
			size_t cpStnodParam = (pStnodParameterList) ? pStnodParameterList->CPStnodChild() : 0;
			auto pSymtab = pParctx->m_pSymtab;

			auto pTinstruct = pSymtab->PTinstructAllocate(istrIdent, cStnodField, cpStnodParam);
			pTinstruct->m_pStnodStruct = pStstruct;

			for ( ; ppStnodMember != ppStnodMemberMax; ++ppStnodMember)
			{
				STNode * pStnodMember = *ppStnodMember;
				if (pStnodMember->m_park != PARK_Decl)
					continue;

				auto pStdecl = PStnodRtiCast<STDecl *>(pStnodMember);
				if (MOE_FVERIFY(pStdecl, "expected stdecl"))
				{
					//if (pStdecl->m_iStnodChildMin == -1)	
					{
						auto pTypememb = pTinstruct->m_aryTypemembField.AppendNew();
						pTypememb->m_pStdecl = pStdecl;
					}
#ifdef MOEB_LATER
					else
					{
						int iStnodChildMax = pStdecl->m_iStnodChildMax;
						for (int iStnod = pStdecl->m_iStnodChildMin; iStnod < iStnodChildMax; ++iStnod)
						{
							auto pTypememb = pTinstruct->m_aryTypemembField.AppendNew();
							pTypememb->m_pStnod = pStnodMember->PStnodChild(iStnod);
						}
					}
#endif
				}

				size_t cTypememb = pTinstruct->m_aryTypemembField.C();
				for (size_t iTypememb = 0; iTypememb < cTypememb; ++iTypememb)
				{
					auto pTypememb = &pTinstruct->m_aryTypemembField[iTypememb];

					auto pStdeclChild = pTypememb->m_pStdecl;
					auto pStnodMemberIdent = pStdeclChild->m_pStnodIdentifier;
					pTypememb->m_istrName = IstrFromIdentifier(pStnodMemberIdent);
				}
			}

			auto pErrman = pParctx->m_pWork->m_pErrman;
			GRFSYM grfsym(FSYM_IsType | FSYM_VisibleWhenNested);
			Symbol * pSymStruct = pSymtabParent->PSymEnsure(pErrman, istrIdent, pStstruct, grfsym, FSHADOW_NoShadowing);
			pStstruct->m_pSymbase = pSymStruct;
			//pSymStruct->m_pTin = pTinstruct;
			pStstruct->m_pTin = pTinstruct;

			CheckTinstructGenerics(pParctx, pStstruct, pTinstruct);

			FExpectConsumeToken(pParctx, pLex, TOK('}'));

			return pStstruct;
		}
		else if (istrRword == RWord::g_istrTypedef)
		{
			InString istrIdent = IstrFromIdentifier(pStnodIdent);

			// create a symbol table for any generic symbols that might be created by this typedef
			SymbolTable * pSymtabTypedef = PSymtabNew(pParctx->m_pAlloc, pParctx->m_pSymtab, istrIdent);
			pSymtabTypedef->m_iNestingDepth = pParctx->m_pSymtab->m_iNestingDepth + 1;
			PushSymbolTable(pParctx, pSymtabTypedef);

			auto pStnodType = PStnodParseTypeSpecifier(pParctx, pLex, "typedef", FPDECL_None);
			ExpectEndOfStatement(pParctx, pLex);

			SymbolTable * pSymtabPop = PSymtabPop(pParctx);
			MOE_ASSERT(pSymtabTypedef == pSymtabPop, "CSymbol table push/pop mismatch (struct)");
			
			if (!pStnodType)
			{
				EmitError(pParctx, LexSpan(pLex), ERRID_TypeSpecifierExpected, "missing type value for typedef %s", istrIdent.m_pChz);

				pParctx->m_pAlloc->MOE_DELETE(pStnodIdent);
				return nullptr;
			}

			auto pStnodTypedef = PStnodAlloc<STNode>(pParctx->m_pAlloc, PARK_Typedef, pLex, LexSpan(pLex));
			pStnodTypedef->m_pSymtab = pSymtabTypedef;

			STNode * aStnod[] = {pStnodIdent, pStnodType};
			pStnodTypedef->CopyChildArray(pParctx->m_pAlloc, aStnod, MOE_DIM(aStnod));

			SymbolTable * pSymtab = pParctx->m_pSymtab;
			auto pErrman = pParctx->m_pWork->m_pErrman;

			GRFSYM grfsym(FSYM_IsType | FSYM_VisibleWhenNested);
			auto pSym = pSymtab->PSymEnsure(pErrman, istrIdent, pStnodTypedef, grfsym, FSHADOW_NoShadowing);
			pStnodTypedef->m_pSymbase = pSym;

			return pStnodTypedef;
		}
	}
	return nullptr;
}

STNode * PStnodExpectCompoundStatement(ParseContext * pParctx, Lexer * pLex, const char * pChzPriorStatement)
{
	if (pLex->m_tok != TOK('{'))
	{
		EmitError(pParctx, LexSpan(pLex), ERRID_CurlyBraceExpected, "Expected '{' after %s'", pChzPriorStatement);

		static const TOK s_aTok[] = {TOK('{'), TOK_EndOfLine };
		LexRecoverAmbit lrecamb(pParctx->m_pLrecst, s_aTok);
		(void) TokSkipToRecovery(pLex, pParctx->m_pLrecst);

		if (pLex->m_tok != TOK('{'))
		{
			// just take a swing at parsing a one line statement - not actually safe, just trying to generate the next error.
			return PStnodParseStatement(pParctx, pLex);
		}
	}

	return PStnodParseCompoundStatement(pParctx, pLex, nullptr);
}

void FinishSwitchList(ParseContext * pParctx, Lexer * pLex, STNode * pStnodList, CDynAry<STNode *> * parypStnodList)
{
	if (!pStnodList)
		return;

	if (!parypStnodList->FIsEmpty())
	{
		pStnodList->CopyChildArray(pParctx->m_pAlloc, parypStnodList->A(), int(parypStnodList->C()));
		parypStnodList->Clear();
	}

	SymbolTable * pSymtabPop = PSymtabPop(pParctx);
	MOE_ASSERT(pStnodList->m_pSymtab == pSymtabPop, "CSymbol table push/pop mismatch (list)");
}

void CreateSwitchList(ParseContext * pParctx, Lexer * pLex, STNode ** ppStnodList, CDynAry<STNode *> * parypStnodList)
{
	auto pStnodList = *ppStnodList;
	FinishSwitchList(pParctx, pLex, pStnodList, parypStnodList);

	pStnodList = PStnodAlloc<STNode>(pParctx->m_pAlloc, PARK_List, pLex, LexSpan(pLex));
	pStnodList->m_tok = TOK('{');
	pStnodList->m_pSymtab = PSymtabNew(pParctx->m_pAlloc, pParctx->m_pSymtab, RWord::g_istrCase);
	PushSymbolTable(pParctx, pStnodList->m_pSymtab);
	*ppStnodList = pStnodList;
}

STNode * PStnodParseSwitchStatement(ParseContext * pParctx, Lexer * pLex)
{
	STValue * pStvalSwitch = PStvalParseReservedWord(pParctx, pLex, RWord::g_istrSwitch);
	STNode * pStnodExp = PStnodParseExpression(pParctx, pLex);

	if (!pStnodExp)
	{
		EmitError(pParctx, LexSpan(pLex), ERRID_SwitchExpressionExpected, "switch statement missing expression");

		pParctx->m_pAlloc->MOE_DELETE(pStvalSwitch);
		return nullptr;
	}

	CDynAry<STNode *> arypStnodSwitch(pParctx->m_pAlloc, BK_Parse);
	CDynAry<STNode *> arypStnodList(pParctx->m_pAlloc, BK_Parse);
	arypStnodSwitch.Append(pStnodExp);

	if (!FTryConsumeToken(pLex, TOK('{')))
	{
		EmitError(pParctx, LexSpan(pLex), ERRID_CurlyBraceExpected, "Expected '{' for case statements'");

		pParctx->m_pAlloc->MOE_DELETE(pStvalSwitch); // pStnodExp will be deleted in dtor
		return nullptr;
	}

	static const TOK s_aTok[] = {TOK('}'), TOK(';'), TOK_EndOfLine };
	LexRecoverAmbit lrecamb(pParctx->m_pLrecst, s_aTok);

	STNode * pStnodList = nullptr;
	bool fInvalidSwitch = false;
	while (pLex->m_tok != TOK('}') && !fInvalidSwitch)
	{
		//RWORD rword = RwordLookup(pLex);
		InString istrRword = pLex->m_istr;
		//switch(rword)
		{
			if (istrRword == RWord::g_istrCase)
			{
				STNode * pStnodCase = PStvalParseReservedWord(pParctx, pLex, RWord::g_istrCase);
				arypStnodSwitch.Append(pStnodCase);

				CDynAry<STNode *> arypStnodCase(pParctx->m_pAlloc, BK_Parse);

				while (1)
				{
					auto pStnodValue = PStnodParseExpression(pParctx, pLex);
					if (!pStnodValue)
					{
						EmitError(pParctx, LexSpan(pLex), ERRID_LabelExpected, "case statement missing it's label");
					}

					arypStnodCase.Append(pStnodValue);

					if (pLex->m_tok != TOK(','))
						break;

					TokNext(pLex);
				}

				FExpectConsumeToken(pParctx, pLex, TOK(':'), "Following 'case' statement");
				CreateSwitchList(pParctx, pLex, &pStnodList, &arypStnodList);

				arypStnodCase.Append(pStnodList);
				pStnodCase->CopyChildArray(pParctx->m_pAlloc, arypStnodCase.A(), int(arypStnodCase.C()));
			}
			else if (istrRword == RWord::g_istrElse)
			{
				STValue * pStnodDefault = PStvalParseReservedWord(pParctx, pLex, RWord::g_istrElse);
				arypStnodSwitch.Append(pStnodDefault);

				FExpectConsumeToken(pParctx, pLex, TOK(':'), "Following switch 'else' statement");
				CreateSwitchList(pParctx, pLex, &pStnodList, &arypStnodList);

				pStnodDefault->CopyChildArray(pParctx->m_pAlloc, pStnodList);
			}
			else
			{
				STNode * pStnod = PStnodParseStatement(pParctx, pLex);
				if (!pStnod)
				{
					EmitError(pParctx, LexSpan(pLex), ERRID_LabelExpected, "missing 'case' or 'default' label");
					fInvalidSwitch = true;
				}
				else
				{
					arypStnodList.Append(pStnod);
				}
			}
		}
	}

	FinishSwitchList(pParctx, pLex, pStnodList, &arypStnodList);
	FExpectConsumeToken(pParctx, pLex, TOK('}'));

	for (int ipStnodCase = 1; ipStnodCase < arypStnodSwitch.C(); ++ipStnodCase)
	{
		STNode * pStnodCase = arypStnodSwitch[ipStnodCase];
		if (!MOE_FVERIFY(pStnodCase, "case should have value, list children"))
			continue;

		int ipStnodList = pStnodCase->CPStnodChild() - 1; 
		auto pStnodListCheck = pStnodCase->PStnodChildSafe(ipStnodList);
		if (pStnodListCheck && pStnodListCheck->m_park == PARK_List && pStnodListCheck->CPStnodChild() == 0)
		{
			EmitError(pParctx, pStnodCase->m_lexsp, ERRID_EmptyCase, 
				"empty switch case must contain at least one statement. Multiple case values are comma separated");
		}
	}

	pStvalSwitch->CopyChildArray(pParctx->m_pAlloc, arypStnodSwitch.A(), int(arypStnodSwitch.C()));
	if (pStvalSwitch->CPStnodChild() < 2)
	{
		EmitError(pParctx, LexSpan(pLex), ERRID_EmptySwitch, "switch statement contains no 'case' or 'default' labels");
		pParctx->m_pAlloc->MOE_DELETE(pStvalSwitch); // pStnodExp will be deleted in dtor
		return nullptr;
	}

	return pStvalSwitch;
}

STNode * PStnodParseJumpStatement(ParseContext * pParctx, Lexer * pLex)
{
	InString istr = pLex->m_istr;
	if (istr == RWord::g_istrContinue || istr == RWord::g_istrBreak || istr == RWord::g_istrFallthrough)
	{
		STNode * pStnod = PStvalParseReservedWord(pParctx, pLex);
		if (pLex->m_tok == TOK_Identifier)
		{
			STValue * pStvalIdent = PStvalParseIdentifier(pParctx, pLex);
			pStnod->CopyChildArray(pParctx->m_pAlloc, pStvalIdent);
		}

		ExpectEndOfStatement(pParctx, pLex);

		return pStnod;
	}
	else if (istr == RWord::g_istrReturn)
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

STNode * PStnodParseSelectionStatement(ParseContext * pParctx, Lexer * pLex, STValue ** ppStvalLabel)
{
	if (pLex->m_istr == RWord::g_istrIf)
	{
		//if expression statement
		//if expression statement else statement

		auto pStvalIf = PStvalParseReservedWord(pParctx, pLex);
		STNode * pStnodExp = PStnodParseExpression(pParctx, pLex);
		if (!pStnodExp)
		{
			EmitError(pParctx, LexSpan(pLex), ERRID_ExpectedExpression, 
				"If statement has no condition expression");
		}

		CDynAry<STNode *> arypStnodIf(pParctx->m_pAlloc, BK_Parse);

		arypStnodIf.Append(pStnodExp);
		//pStvalIf->CopyChildArray(pParctx->m_pAlloc, pStnodExp);
		
		STNode * pStnodStatement = PStnodExpectCompoundStatement(pParctx, pLex, RWord::g_pChzIf);
		if (!pStnodStatement)
		{
			EmitError(pParctx, LexSpan(pLex), ERRID_UnexpectedToken, 
				"Error parsing if statement. unexpected token '%s'", PChzFromTok((TOK)pLex->m_tok));

			// move the lexer forward until it has some hope of generating decent errors
			static const TOK s_aTok[] = {TOK(';'), TOK('{') };
			SkipToToken(pLex, s_aTok, MOE_DIM(s_aTok), FLEXER_EndOfLine);
		}
		else
		{
			if (pStnodStatement->m_grfstnod.FIsSet(FSTNOD_EntryPoint))
			{
				MOE_ASSERT(pStnodStatement->m_park == PARK_ProcedureDefinition, "Unknown entry point park");
				EmitError( pParctx, LexSpan(pLex), ERRID_CurlyBraceExpected, "Local functions not directly allowed under conditional, add {}");
			}
			else
			{
				arypStnodIf.Append(pStnodStatement);
			}

			//RWORD rwordElse = RwordLookup(pLex);
			if (pLex->m_istr == RWord::g_istrElse)
			{
				STNode * pStnodElse = PStvalParseReservedWord(pParctx, pLex);

				InString istrRword = pLex->m_istr;
				STNode * pStnodStatement = (istrRword == RWord::g_istrIf) ? 
												PStnodParseSelectionStatement(pParctx, pLex, nullptr) :
												PStnodExpectCompoundStatement(pParctx, pLex, istrRword.m_pChz);

				if (pStnodStatement->m_grfstnod.FIsSet(FSTNOD_EntryPoint))
				{
					MOE_ASSERT(pStnodStatement->m_park == PARK_ProcedureDefinition, "Unknown entry point park");
					EmitError( pParctx, LexSpan(pLex), ERRID_CurlyBraceExpected, 
						"Local functions not directly allowed under conditional, add {}");
				}
				else
				{
					pStnodElse->CopyChildArray(pParctx->m_pAlloc, pStnodStatement);
					arypStnodIf.Append(pStnodElse);
				}
			}
		}

		pStvalIf->CopyChildArray(pParctx->m_pAlloc, arypStnodIf.A(), int(arypStnodIf.C()));
		return pStvalIf;
	}

	if (pLex->m_istr == RWord::g_istrSwitch)
	{
		auto pStnodSw = PStnodParseSwitchStatement(pParctx, pLex);

#ifdef MOEB_LATER
		// uh, how do we handle labels now that m_pStident is no longer a thing
		if (ppStvalLabel)
		{
			MOE_ASSERT(!pStnodSw->m_pStident, "expected null identifier");
			pStnodSw->m_pStident = *ppStvalLabel;
			*ppStvalLabel = nullptr;
		}
#endif

		return pStnodSw;

	}
	return nullptr;
}

STNode * PStnodParseIterationStatement(ParseContext * pParctx, Lexer * pLex, STValue ** ppStvalLabel)
{
	InString istrRword = pLex->m_istr;
	if (istrRword == RWord::g_istrFor)
	{
		STFor * pStfor = PStnodAlloc<STFor>(pParctx->m_pAlloc, PARK_For, pLex, LexSpan(pLex));
		TokNext(pLex);
		//STValue * pStvalFor = PStvalParseReservedWord(pParctx, pLex);

		LexSpan lexsp(pLex);
		if (pLex->m_tok == TOK('('))
		{
			EmitError(pParctx->m_pWork, lexsp, ERRID_OldCStyle, "Parens are not needed for C-Style for loop");
			TokNext(pLex);
		}

		//auto pStfor = pStvalFor->PStmapEnsure<CSTFor>(pParctx->m_pAlloc);

		if (ppStvalLabel)
		{
#ifdef MOEB_LATER
			pStvalFor->m_pStident = *ppStidentLabel;
#endif
			*ppStvalLabel = nullptr;
		}

		SymbolTable * pSymtabLoop = PSymtabNew(pParctx->m_pAlloc, pParctx->m_pSymtab, RWord::g_istrFor);
		pStfor->m_pSymtab = pSymtabLoop;

		PushSymbolTable(pParctx, pSymtabLoop);

		GRFPDECL grfpdecl = FPDECL_AllowUninitializer | FPDECL_AllowCompoundDecl;
		auto * pStnodDecl =  PStnodParseParameter(pParctx, pLex, pParctx->m_pSymtab, grfpdecl);
		if (pStnodDecl)
			ExpectEndOfStatement(pParctx, pLex);
		else
		{
			pStnodDecl = PStnodParseExpression(pParctx, pLex);
			FExpectConsumeToken(pParctx, pLex, TOK(';'));
		}
		pStfor->m_pStnodDecl = pStnodDecl;

		STNode * pStnodPred = PStnodParseExpression(pParctx, pLex);
		if (pStnodPred)
			ExpectEndOfStatement(pParctx, pLex);
		else
			FExpectConsumeToken(pParctx, pLex, TOK(';'));
		pStfor->m_pStnodPredicate = pStnodPred;

		STNode * pStnodIncrement = PStnodParseExpression(pParctx, pLex);
		if (pStnodIncrement)
			ExpectEndOfStatement(pParctx, pLex);
		else
			FExpectConsumeToken(pParctx, pLex, TOK(';'));
		pStfor->m_pStnodIncrement = pStnodIncrement;

		STNode * pStnodBody = PStnodExpectCompoundStatement(pParctx, pLex, RWord::g_pChzFor);
		pStfor->m_pStnodBody = pStnodBody;

		SymbolTable * pSymtabPop = PSymtabPop(pParctx);
		MOE_ASSERT(pSymtabLoop == pSymtabPop, "CSymbol table push/pop mismatch (list)");

		return pStfor;
	}

	if (istrRword == RWord::g_istrWhile)
	{
		STValue * pStvalWhile = PStvalParseReservedWord(pParctx, pLex);
		STNode * pStnodExp = PStnodParseExpression(pParctx, pLex);
		
		if (ppStvalLabel)
		{
#ifdef MOEB_LATER
			pStvalWhile->m_pStident = *ppStidentLabel;
#endif
			*ppStvalLabel = nullptr;
		}
		
		STNode * pStnodStatement = PStnodExpectCompoundStatement(pParctx, pLex, RWord::g_pChzWhile);

		STNode * apStnod[] = {pStnodExp, pStnodStatement};	
		pStvalWhile->CopyChildArray(pParctx->m_pAlloc, apStnod, MOE_DIM(apStnod));
		return pStvalWhile;
	}

	return nullptr;
}


STNode * PStnodParseStatement(ParseContext * pParctx, Lexer * pLex)
{
	STNode * pStnod = PStnodParseCompoundStatement(pParctx, pLex, nullptr);
	if (pStnod)
		return pStnod;

	// Note - Declarations and definition checks need to come first because they peek ahead to see 
	//  if an identifier has struct, proc : or :=

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
	STValue * pStvalLabel = nullptr;

	if (FTryConsumeToken(pLex, TOK_Label))
	{
		if (pLex->m_tok != TOK_Identifier)
		{
			EmitError(pParctx, LexSpan(pLex), ERRID_MissingLabel, "Encountered Label directive without label string");
		}
		else
		{
			pStvalLabel = PStvalParseIdentifier(pParctx, pLex);
		}
	}

	pStnod = PStnodParseSelectionStatement(pParctx, pLex, &pStvalLabel);
	
	if (!pStnod)
	{
		pStnod = PStnodParseIterationStatement(pParctx, pLex, &pStvalLabel);
	}

	if (pStvalLabel)
	{
		EmitError(pParctx, pStvalLabel->m_lexsp, ERRID_FloatingLabel, "Label directive should precede loop or switch statement.");
		pParctx->m_pAlloc->MOE_FREE(pStvalLabel);
	}

	if (pStnod)
		return pStnod;

	return PStnodParseJumpStatement(pParctx, pLex);
}

bool FParseImportDirectives(ParseContext * pParctx, Lexer * pLex)
{
	if (pLex->m_tok != TOK_Identifier)
		return false;
	
	Workspace * pWork = pParctx->m_pWork;
	Workspace::FILEK filek;
	Moe::InString istrDirective = pLex->m_istr;
	if (istrDirective == RWord::g_istrImportDirective)
	{
		filek = Workspace::FILEK_Source;
	}
	else if (istrDirective == RWord::g_istrForeignLibraryDirective)
	{
		filek = Workspace::FILEK_ForeignLibrary;
	}
	else if (istrDirective == RWord::g_istrStaticLibraryDirective)
	{
		filek = Workspace::FILEK_StaticLibrary;
	}
	else if (istrDirective == RWord::g_istrDynamicLibraryDirective)
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
		(void) pWork->PFileEnsure(pLex->m_istr, filek);

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

void ExecuteParseJob(Compilation * pComp, Workspace * pWork, Job * pJob)
{
	auto pParjd = (ParseJobData *)pJob->m_pVData;
	ParseContext * pParctx = &pParjd->m_parctx;
	Lexer * pLex = &pParjd->m_lex;

	LexRecoverAmbit lrecamb(pParctx->m_pLrecst, TOK(';'), TOK_EndOfLine);
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
			(void)TokSkipToRecovery(pLex, pParctx->m_pLrecst);
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
					PChzLongFromPark(pStnod->m_park));
			}
		}
#endif
	}
}

void CleanupParseJob(Workspace * pWork, Job * pJob)
{
	if (pJob->m_pVData)
	{
		auto pParjd = (ParseJobData *)pJob->m_pVData;
		pWork->m_pAlloc->MOE_DELETE(pParjd);
		pJob->m_pVData = nullptr;
	}
}

Job * PJobCreateParse(Compilation * pComp, Workspace * pWork, const char * pChzBody, Moe::InString istrFilename)
{
	Alloc * pAlloc = pWork->m_pAlloc;
	ParseJobData * pParjd = MOE_NEW(pAlloc, ParseJobData) ParseJobData(pAlloc, pWork);

	InitLexer(&pParjd->m_lex, pChzBody, &pChzBody[CBChz(pChzBody)-1], pParjd->m_aChStorage, sizeof(pParjd->m_aChStorage));

	pParjd->m_pChzBody = pChzBody;
	pParjd->m_lex.m_istrFilename = istrFilename;

	// not thread safe?
	//PushSymbolTable(, pWork->m_pSymtab);

	ParseContext * pParctx = &pParjd->m_parctx;
	MOE_ASSERT(pParctx->m_pSymtab == nullptr, "expected null top-level symbol table");
	pParctx->m_pSymtab = pWork->m_pSymtab;

	auto pJob = PJobAllocate(pComp, pParjd);
	pJob->m_pFnUpdate = ExecuteParseJob;
	pJob->m_pFnCleanup = CleanupParseJob;
	EnqueueJob(pComp, pJob);

	return pJob;
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

