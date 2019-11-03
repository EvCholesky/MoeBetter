
#include <EwcArray.h>

enum REQK // REQuest Kind
{
	REQK_FindSymbol,		// lookup a symbol by name
	REQK_FindAst,			// lookup an ast
	REQK_FindTypeInfo,		// find type info (from symbol)
	REQK_FindDefinition,	// where was this instance defined
	REQK_FindReferences,	// where was this symbol referenced
	REQK_ListMembers,		// what are the members for this struct
	REQK_BytecodeEntry,		// bytecode entry to run a given procedure
	REQK_CodegenResult,		// object file?

	/* requests should be desired results not processes needed to return generate those results
    REQK_BuildAst,			// lex and parse a source file
    REQK_TypeCheckSymbol,	
    REQK_CodegenBytecode,	
    REQK_CodegenNative,
	*/
};

// indices
enum REQPARMK
{
	REQPARMK_SymbolPath,
	REQPARMK_SourceLocation,
};

struct ReqSymbolPath // tag = rqsympath
{
	CString		m_strSymPath;
};

struct ReqSourceLocation // tag = rqsrcloc
{
	CString		m_strFilename;
	s32			m_iLine;
	s32			m_iCodepoint;
};

struct RequestParm // tag = rqparam
{
	REQPARMK	m_reqparmk;
	union
	{
		ReqSymbolPath		m_rqsympath;
		ReqSourceLocation	m_rqsrcloc;
	};
};

// lookup by symbol path string:		math.vec2.m_x
// lookup by source location			array.moe:12,2

struct RequestResult // tag = rqresult
{
};

struct Request // tag = rq
{
	REQK					m_reqk;
	RequestParm				m_rqparm;
};

struct Compilation 
{
	void AddRequest(Request * rq);
	void ServiceRequest(CDynAry<RequestResult> * paryRqresult);
};

/*
struct SReqEntry    // tag = reqent
{
    REQENTK     m_reqentk;
    CString     m_strName;
};

struct SCompilation // tag = comp
{
    EWC::CDynAry<SReqEntry *>       m_arypReqent;
    EWC::CDynAry<SReqAST *>         m_aryReqast;
    SReqSymTable *                  m_pSymtabGlobal;
};

void CompileSource(const char * pChzFilename, SCompilation * pComp);
void RequestCompilation(SCompilation * pComp, REQK reqk);

SReqSymbol * PReqsymLookup(const char * pChzName);
SReqEntry * PReqentTypeOf(SReqSymbol * pReqsym);
*/