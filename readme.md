
An experimental sketch version of the Moe compiler. (I'm trying to take the patient apart, rearrange it and put it back together without killing it)


Changes:

Query based compilation  

Multi-threaded

Parsing should try to recover from errors as soon as possible and resume parsing

AST
	* ast tracks whitespace, should be able to reproduce original source

String -> Using interred strings, just check pointer rather than HV, threads will need to use mutex when adding to the hash

TypeInfo 
	* All types should be unique from the start, don't set an AST node's type until it can be uniquely determined 

collapse STypeInfoInt and STypeInfoFloat to STypeInfoNumeric 

Notation changes
	* no leading S/C for structs or classes
	* EWC macros/namespaces should change go MOE
	* No more pCoz/pChz, all string code should handle UTF8

Things I contemplated changing, but didn't
Alloc -> not using arena based loading - this would be nicer, but I don't think it will work with caching query results

SLexerLocation -> LexerSpan; should always be passed as const &

removing
acast 


TODO:
[] "array references" -> array slice
[] add module support
