#pragma once

#include "Error.h"
#include "MoeTypes.h"

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

struct MoeQuery // tag = mq
{
};

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

		s32				m_dBWarm;		// byte delta for warm start (previous lookup)
		s32				m_iLineWarm;	// warm start line	
		s32				m_iColumnWarm;	// warm start col
	};

	char *				PChzLoadFile(const Moe::InString & istrFilename, Moe::Alloc * pAlloc);
	void				AppendEntry(STNode * pStnod, SymbolTable * pSymtab);

	File *				PFileEnsure(const char * pChzFile, FILEK filek);
};
