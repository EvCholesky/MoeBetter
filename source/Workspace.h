#pragma once

#include "Error.h"
#include "MoeHash.h"
#include "MoeString.h"
#include "MoeTypes.h"

struct Lexer;
struct ParseContext;
struct STNode;
struct SymbolTable;
struct TypeRegistry;
struct Workspace;
struct DIFile;

enum FUNT	// Flag UNit Tests
{
	FUNT_ImplicitProc		= 0x1,		// wrap the code in an implicit procedure for testing
										//   if this is not set it will test as a global
	FUNT_ResolveAllSymbols	= 0x2,		// mark all symbols as in use, don't search for main()

	FUNT_None				= 0x0,
	FUNT_All				= 0x3,

	GRFUNT_Default			= FUNT_ResolveAllSymbols,
	GRFUNT_DefaultTest      = FUNT_ImplicitProc | FUNT_ResolveAllSymbols,
};

MOE_DEFINE_GRF(GRFUNT, FUNT, u32)

enum OPTLEVEL
{
	OPTLEVEL_Debug,
	OPTLEVEL_Release,
};

enum TARGETOS
{
	TARGETOS_Nil = -1,
	TARGETOS_Windows,
};

struct UniqueNameSet // tag = unset
{
							UniqueNameSet(Moe::Alloc * pAlloc, Moe::BK bk, u32 cCapacityStarting = 32)
							:m_hashHvNUnique(pAlloc, bk, cCapacityStarting)
								{ ; }
	void					Clear(u32 cCapacity)
								{ m_hashHvNUnique.Clear(cCapacity); }

	Moe::CHash<HV, u32>		m_hashHvNUnique;		// map for generating unique strings
};

extern void GenerateUniqueName(UniqueNameSet * pUnset, const char * pChzIn, char * pChzOut, size_t cBOutMax);

struct WorkspaceEntry // tag = entry
{
							WorkspaceEntry()
							:m_pStnod(nullptr)
							,m_pSymtab(nullptr)
							,m_fHideDebugString(false)
								{ ; }

	STNode *				m_pStnod;
	SymbolTable *		 	m_pSymtab;	// symbol table for this entry, local symbols for lambdas 

	bool					m_fHideDebugString;	// don't print during WriteDebugStringForEntries()
};

typedef Moe::CBlockList<WorkspaceEntry, 128> BlockListEntry;

struct Workspace	// tag = work
{
	enum FILEK
	{
		FILEK_Source,
		FILEK_UnitTest,
		FILEK_ForeignLibrary,
		FILEK_StaticLibrary,
		FILEK_DynamicLibrary,

		FILEK_Max,
		FILEK_Min = 0,
		FILEK_Nil = -1,
	};

	enum FILES
	{
		FILES_Nil,
		FILES_Requested,
		FILES_Processing,
		FILES_Complete,
	};

	static const int	s_cBFilenameMax = 1024;
	static const char * s_pChzSourceExtension;
	static const char * s_pChzUnitTestExtension;

	struct File // tag = file
	{
						File(const Moe::InString & istrFilename, FILEK filek)
						:m_istrFilename(istrFilename)
						,m_pChzFileBody(nullptr)
						,m_pDif(nullptr)
						,m_filek(filek)
						,m_files(FILES_Requested)
						,m_dBWarm(0)
						,m_iLineWarm(0)
						,m_iColumnWarm(0)
							{ ; }

		Moe::InString	m_istrFilename;	// full filename;
		const char *	m_pChzFileBody;	// contents of the file
		DIFile *		m_pDif;			
		FILEK			m_filek;
		FILES			m_files;

		s64				m_dBWarm;		// byte delta for warm start (previous lookup)
		s64				m_iLineWarm;	// warm start line	
		s64				m_iColumnWarm;	// warm start col
	};

						Workspace(Moe::Alloc * pAlloc, ErrorManager * pErrman);

	void				CopyUnitTestFiles(Workspace * pWorkOther);
	char *				PChzLoadFile(const Moe::InString & istrFilename, Moe::Alloc * pAlloc);
	void				AppendEntry(STNode * pStnod, SymbolTable * pSymtab);

	File *				PFileEnsure(const Moe::InString istrFilename, FILEK filek);
	File *				PFileLookup(const char * pChzFile, FILEK filek);
	Moe::CHash<HV, int> * 
						PHashHvIPFile(FILEK filek);

	Moe::Alloc *						m_pAlloc;
	BlockListEntry 						m_blistEntry;
	Moe::CDynAry<WorkspaceEntry *> 		m_arypEntryChecked;		// order in which entry points were successfully type checked

	typedef Moe::CHash<HV, int> HashHvIPFile;
	Moe::CHash<HV, int> *				m_mpFilekPHashHvIPFile[FILEK_Max];
	Moe::CDynAry<File *> 				m_arypFile;
	const char *						m_pChzObjectFilename;

	SymbolTable *						m_pSymtab;				// top level symbols
	TypeRegistry *						m_pTyper;
	UniqueNameSet						m_unset;
	UniqueNameSet						m_unsetTin;				// unique names used by types

	ErrorManager *						m_pErrman;
	size_t								m_cbFreePrev;

	TARGETOS							m_targetos;
	OPTLEVEL							m_optlevel;
	GRFUNT								m_grfunt;
};

enum FCCOL // Console color
{
	FCCOL_FgBlue	= 0x1,
	FCCOL_FgGreen	= 0x2,
	FCCOL_FgRed		= 0x4,
	FCCOL_FgIntense = 0x8,
	FCCOL_BgBlue	= 0x10,
	FCCOL_BgGreen	= 0x20,
	FCCOL_BgRed		= 0x40,
	FCCOL_BgIntense = 0x80,

	FCCOL_None		= 0x0,
	FCCOL_All		= 0xFF,

	GRFCCOL_FgIntenseRed		= FCCOL_FgRed | FCCOL_FgIntense,
	GRFCCOL_FgIntenseBlue		= FCCOL_FgBlue | FCCOL_FgIntense,
	GRFCCOL_FgIntenseYellow		= FCCOL_FgRed | FCCOL_FgGreen | FCCOL_FgIntense,
	GRFCCOL_FgIntenseWhite		= FCCOL_FgRed | FCCOL_FgGreen | FCCOL_FgBlue | FCCOL_FgIntense,

	GRFCCOL_FgAllColor			= FCCOL_FgRed | FCCOL_FgGreen | FCCOL_FgBlue,
	GRFCCOL_FgAll				= GRFCCOL_FgAllColor | FCCOL_FgIntense,
	GRFCCOL_BgAllColor			= FCCOL_BgRed | FCCOL_BgGreen | FCCOL_BgBlue,
	GRFCCOL_BgAll				= GRFCCOL_BgAllColor | FCCOL_BgIntense,
};

enum FCOMPILE
{
	FCOMPILE_PrintIR	= 0x1,
	FCOMPILE_FastIsel	= 0x2,
	FCOMPILE_Native		= 0x4,
	FCOMPILE_Bytecode	= 0x8,

	FCOMPILE_None		= 0x0,
	FCOMPILE_All		= 0xF,
};

MOE_DEFINE_GRF(GRFCOMPILE, FCOMPILE, u32);

MOE_DEFINE_GRF(GRFCCOL, FCCOL, u16);

void InitConsoleSettings();
GRFCCOL GrfccolCurrent();
void ResetConsoleTextColor();

struct ConsoleColorAmbit // tag = ccolamb
{
					ConsoleColorAmbit()
						{ m_grfccol = GrfccolCurrent(); }
					~ConsoleColorAmbit()
						{ ResetConsoleTextColor(); }

	void			SetConsoleForegroundColor(GRFCCOL grfccol);
	void			SetConsoleBackgroundColor(GRFCCOL grfccol);
		
	GRFCCOL			m_grfccol;
};

void SetConsoleTextColor(GRFCCOL grfccol);

void BeginWorkspace(Workspace * pWork);
void BeginParse(Workspace * pWork, Lexer * pLex, const char * pChzIn, const char * pChzFilename = nullptr);
void EndParse(Workspace * pWork, Lexer * pLex);
void EndWorkspace(Workspace * pWork);

struct LexLookup // tag = lexlook
{
					LexLookup()
					:m_istrFilename()
					,m_iLine(0)
					,m_iCodepoint(0)
						{ ; }

					LexLookup(Workspace * pWork, const LexSpan & pLexsp);
					LexLookup(Workspace * pWork, STNode * pStnod);

	Moe::InString	m_istrFilename;
	s64				m_iLine;
	s64				m_iCodepoint;
};
