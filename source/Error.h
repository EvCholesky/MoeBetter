/* Copyright (C) 2017 Evan Christensen
|
| Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated 
| documentation files (the "Software"), to deal in the Software without restriction, including without limitation the 
| rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit 
| persons to whom the Software is furnished to do so, subject to the following conditions:
| 
| The above copyright notice and this permission notice shall be included in all copies or substantial portions of the 
| Software.
| 
| THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE 
| WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
| COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR 
| OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. */

#pragma once

#include "MoeArray.h"
#include <stdarg.h>

namespace Moe
{
	struct Alloc;
}

struct GenericMap;
struct LexSpan;
struct ParseContext;
struct TestContext;
struct TypeCheckContext;
struct Workspace;

enum ERRID
{
	ERRID_UnknownError				= 0,
	ERRID_FailedOpeningFile 		= 1,
	ERRID_MissingEntryPoint			= 2,
	ERRID_FailedLoadingDLL			= 3,
	ERRID_UnhandledRWord			= 4,

	ERRID_ParserMin					= 1000,
	ERRID_EnumRepeat				= 1001,
	ERRID_ShadowedDefine			= 1002,
	ERRID_InvalidOpOverload			= 1003,
	ERRID_OldCStyle					= 1004,
	ERRID_EmptyCase					= 1005,
	ERRID_DefaultParamOpOverload	= 1006, 
	ERRID_MissingDefaultArgs		= 1007,
	ERRID_MissingName				= 1008,
	ERRID_NoGenericReturn			= 1009,
	ERRID_MultipleAnchorDef			= 1010,
	ERRID_UsingStatementNotAllowed	= 1011,
	ERRID_BadUsingSyntax			= 1012,
	ERRID_ExpectedEndOfLine			= 1013,
	ERRID_UnexpectedToken			= 1014,
	ERRID_TypeSpecParseFail			= 1015,
	ERRID_NonBakedStructParameter   = 1016,
	ERRID_EmptyStruct				= 1017,
	ERRID_GenericDeclNotAllowed		= 1018,
	ERRID_MissingPath				= 1019,
	ERRID_MissingLabel				= 1020,
	ERRID_FloatingLabel				= 1021,
	ERRID_MissingRWord				= 1022,
	ERRID_UnknownDirective			= 1023,
	ERRID_MissingRhs				= 1024,
	ERRID_MissingOperand			= 1025,
	ERRID_MissingToken				= 1026,
	ERRID_FeatureNotImplemented		= 1027,
	ERRID_ExpectedExpression		= 1028,
	ERRID_MissingIdentifier			= 1029,
	ERRID_CStyle					= 1030,
	ERRID_UninitializerNotAllowed	= 1031,
	ERRID_ImmutableNotAllowed		= 1032,
	ERRID_CompoundDeclNotAllowed	= 1033,
	ERRID_InitialValueExpected		= 1034,
	ERRID_TypeSpecifierExpected		= 1035,
	ERRID_VariadicMustBeLast		= 1036,
	ERRID_ExpectedParameter			= 1037,
	ERRID_NoReturnStatement			= 1038,
	ERRID_BadStructGenericParam		= 1039,
	ERRID_OnlyOperatorCanCommute	= 1040,
	ERRID_ForeignProcDefinesBody	= 1041,
	ERRID_ProcBodyExpected			= 1042,
	ERRID_OnlyTwoArgsCommute		= 1043,
	ERRID_StructureParamsExpected	= 1044,
	ERRID_EnumConstantExpected		= 1044,
	ERRID_CurlyBraceExpected		= 1045,
	ERRID_SwitchExpressionExpected	= 1046,
	ERRID_EmptySwitch				= 1046,
	ERRID_LabelExpected				= 1047,
	ERRID_ParserMax					= 2000,

	ERRID_TypeCheckMin				= ERRID_ParserMax,
	ERRID_InitTypeMismatch			= 2001,
	ERRID_TooFewArgs				= 2002,
	ERRID_TooManyArgs				= 2003,
	ERRID_NotLvalue					= 2004,
	ERRID_NotRvalue					= 2005,
	ERRID_BadImplicitConversion		= 2006,
	ERRID_CantFindProc				= 2007,
	ERRID_CantFindMain				= 2008,
	ERRID_IncorrectIvalk			= 2009,
	ERRID_BadArrayIndex				= 2010,
	ERRID_NotYetSupported			= 2011,
	ERRID_AmbiguousOverload			= 2012,
	ERRID_BadOverloadSig			= 2013,
	ERRID_ArgumentSuppliedTwice		= 2014,
	ERRID_NamedArgumentNotFound		= 2015,
	ERRID_GenericLookupFail			= 2016,
	ERRID_NoGenericRValue			= 2017,
	ERRID_BakingNonLiteralValue		= 2018,
	ERRID_StructParamsMustBeBaked	= 2019,
	ERRID_BadMemberLookup			= 2020,
	ERRID_CannotTakeReference		= 2021,
	ERRID_InvalidUnaryOp			= 2022,
	ERRID_UsingStatementCycle		= 2023,
	ERRID_UsingStatementCollision	= 2024,
	ERRID_UsingStatementBadType		= 2025,
	ERRID_LiteralMemberNotFound		= 2026,
	ERRID_LiteralUnnamedMember		= 2027,
	ERRID_NonConstantInLiteral		= 2028,
	ERRID_UninstantiableType		= 2029,
	ERRID_OperatorNotDefined		= 2030,
	ERRID_CannotInferGeneric		= 2031,
	ERRID_OrderedAfterNamed         = 2032,
	ERRID_UnresolvedTypeRef			= 2033,
	ERRID_UnexpectedSymbolDef		= 2034,
	ERRID_UnknownIdentifier			= 2035,
	ERRID_GenericArgsExpected		= 2036,
	ERRID_CannotConvertToNull		= 2037,
	ERRID_InvalidCast				= 2038,
	ERRID_LitOverflow				= 2039,
	ERRID_InvalidArgument			= 2040,
	ERRID_ReturnTypeExpected		= 2041,
	ERRID_InvalidLabel				= 2042,
	ERRID_TooManyInitializers		= 2043,
	ERRID_CannotInferType			= 2044,
	ERRID_UnnamedNotAllowed			= 2045,
	ERRID_LiteralOutsideBounds		= 2046,
	ERRID_GenericArgsMustBeFirst	= 2047,
	ERRID_MissingSymbolDef			= 2048,
	ERRID_MissingMemberDef			= 2049,
	ERRID_UnknownSymbol				= 2050,
	ERRID_TypeExpected				= 2051,
	ERRID_LhsHasNoEffect			= 2052,
	ERRID_SymbolLookupFailed		= 2053,
	ERRID_CannotConvertToBool		= 2054,
	ERRID_CannotDereference			= 2055,
	ERRID_MalformedAstInTypeCheck	= 2056,
	ERRID_StatementAfterFallthrough = 2057,
	ERRID_SwitchCaseMustBeInteger	= 2058,
	ERRID_SwitchElseAlreadyDefined  = 2059,
	ERRID_MissingPredicate			= 2060,
	ERRID_ReturnOutsideProcedure	= 2061,
	ERRID_NonVoidReturnExpected		= 2062,
	ERRID_CaseAlreadyUsed			= 2063,
	ERRID_NoAssignmentOp			= 2064,
	ERRID_UnknownArrayMember		= 2065,
	ERRID_BadFlagEnumAccess			= 2066,
	ERRID_RhsIdentifierExpected		= 2067,
	ERRID_TypeCheckMax				= 3000,

	ERRID_CodeGenMin				= ERRID_TypeCheckMax,
	ERRID_UnreachableInst			= 3001,
	ERRID_BadStore					= 3002,
	ERRID_BadCastGen				= 3003,
	ERRID_ObjFileFail				= 3004,
	ERRID_ZeroSizeInstance			= 3005,
	ERRID_UndefinedForeignFunction  = 3006,
	ERRID_CodeGenMax				= 4000,

	ERRID_TestMin					= ERRID_CodeGenMax,
	ERRID_MissingQuote				= 4001,
	ERRID_CantFindSubstitution		= 4002,
	ERRID_NoRecursiveSubstitute		= 4003,
	ERRID_ErridExpected				= 4004,
	ERRID_TooManyErrids				= 4005,
	ERRID_StringLiteralExpected		= 4006,
	ERRID_OptionStringExpected		= 4007,
	ERRID_ExpectedTestVar			= 4008,
	ERRID_PermutationExpected		= 4009,
	ERRID_TestExpected				= 4010,
	ERRID_NotPermuteTest			= 4011,
	ERRID_TestMax					= 5000,
	ERRID_ErrorMax					= 10000,

	ERRID_WarningMin				= 10000,
	ERRID_UnknownWarning			= 10000,
	ERRID_WarningMax				= 20000,


	ERRID_Max,
	ERRID_Min = 0,
	ERRID_Nil = -1
};

struct ErrorCount	// tag = errc
{
			ErrorCount(ERRID errid)
			:m_errid(errid)
			,m_c(0)
				{ ; }

	ERRID	m_errid;
	int		m_c;
};

struct ErrorManager	//  // tag = errman
{
				ErrorManager(Moe::Alloc * pAlloc);

	void		SetWorkspace(Workspace * pWork)
					{ m_pWork = pWork; }

	void		AddChildErrors(const ErrorManager * pErrmanOther)
					{
						auto paryErridOther = &pErrmanOther->m_aryErrid;
						m_aryErrid.Append(paryErridOther->A(), paryErridOther->C());
					}
	bool		FHasHiddenErrors();
	bool		FHasErrors()
					{ return CError() != 0; }
	void		Clear()
					{ m_aryErrid.Clear(); }

	int			CError()
					{	
						int cError, cWarning; 
						ComputeErrorCounts(&cError, &cWarning);
						return cError;
					}
	int			CWarning()
					{	
						int cError, cWarning; 
						ComputeErrorCounts(&cError, &cWarning);
						return cWarning;
					}
	void		ComputeErrorCounts(int * pCError, int * pCWarning);
	bool		FTryHideError(ERRID errid);

	void		PushGenmapContext(GenericMap * pGenmap);
	void		PopGenmapContext(GenericMap * pGenmap);

	Workspace *					m_pWork;				// back pointer for SFile lookup inside EmitError
	Moe::CDynAry<ERRID>			m_aryErrid;				// numbered errors (for expected unit test errors)

	Moe::CDynAry<ErrorCount> *	m_paryErrcExpected;
	Moe::CDynAry<GenericMap *>	m_arypGenmapContext;	// instantiate context
};

enum ERRS
{
	ERRS_Unreported,
	ERRS_Hidden,
	ERRS_Reported,
};

struct Error 
{
						Error(ErrorManager * pErrman, ERRID errid = ERRID_UnknownError);

	ErrorManager *		m_pErrman;
	ERRID				m_errid;
	ERRS				m_errs;
};

void EmitWarning(ErrorManager * pErrman, const LexSpan & lexsp, ERRID errid, const char * pChz, va_list ap);
void EmitWarning(ErrorManager * pErrman, const LexSpan & lexsp, ERRID errid, const char * pChz, ...);
void EmitWarning(Workspace * pWork, const LexSpan & lexsp, ERRID errid, const char * pChz, va_list ap);
void EmitWarning(Workspace * pWork, const LexSpan & lexsp, ERRID errid, const char * pChz, ...);
void EmitWarning(ParseContext * pParctx, const LexSpan & lexsp, ERRID errid, const char * pChz, va_list ap);
void EmitWarning(ParseContext * pParctx, const LexSpan & lexsp, ERRID errid, const char * pChz, ...);
void EmitWarning(TypeCheckContext * pTcwork, const LexSpan & lexsp, ERRID errid, const char * pChz, va_list ap);
void EmitWarning(TypeCheckContext * pTcwork, const LexSpan & lexsp, ERRID errid, const char * pChz, ...);

void EmitError(ErrorManager * pErrman, const LexSpan & lexsp, ERRID errid, const char * pChz, va_list ap);
void EmitError(ErrorManager * pErrman, const LexSpan & lexsp, ERRID errid, const char * pChz, ...);
void EmitError(Workspace * pWork, const LexSpan & lexsp, ERRID errid, const char * pChz, va_list ap);
void EmitError(Workspace * pWork, const LexSpan & lexsp, ERRID errid, const char * pChz, ...);
void EmitError(ParseContext * pParctx, const LexSpan & lexsp, ERRID errid, const char * pChz, va_list ap);
void EmitError(ParseContext * pParctx, const LexSpan & lexsp, ERRID errid, const char * pChz, ...);
void EmitError(TypeCheckContext * pTcwork, const LexSpan & lexsp, ERRID errid, const char * pChz, va_list ap);
void EmitError(TypeCheckContext * pTcwork, const LexSpan & lexsp, ERRID errid, const char * pChz, ...);

void PrintErrorLine(Error * pError, const char * pChzPrefix, const LexSpan & lexsp, const char * pChz, va_list ap);
void PrintErrorLine(Error * pError, const char * pChzPrefix, const LexSpan & lexsp, const char * pChz, ...);

enum ERREP // ERror REPorting
{
	ERREP_HideErrors,
	ERREP_ReportErrors,	
};

