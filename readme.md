
An experimental sketch version of the Moe compiler. (I'm trying to take the patient apart, rearrange it and put it back together without killing it)


## Changes:

* Recover from errors rather than aborting on first one
* Query based compilation  
* Multi-threaded

### AST

* String -> Using interred strings, just check pointer rather than HV, threads will need to use mutex when adding to the hash

* SLexerLocation -> LexerSpan; should always be passed as const &

### TypeInfo 
* All types should be unique from the start, don't set an AST node's type until it can be uniquely determined 

* Collapse TypeInfoInt and TypeInfoFloat to TypeInfoNumeric 

## Notation changes
* No leading S/C for structs or classes
* EWC macros/namespaces changed to MOE
* No more pCoz/pChz, all string code should handle UTF8

## Things I contemplated changing, but didn't
* Alloc -> not using arena based loading - this would be nicer, but I don't think it will work with caching query results

## Actual language changes
* Removing acast 
* Removing for_each
* Using unnamed types require a `:` before the type

## Multithreading concerns
* Each thread has write access to the AST in it's working set (parser=file, typecheck=WorkspaceEntry) and any cross set searches (symbol lookup, registry) needs to be guarded
[ ] change the type creation routines to go through a typeFactory that will guarantee uniqueness and control thread access

## Compile-time execution
[ ] symbols should be defined during parse and then symbol tables should be locked
* Not all files will parse before we start type checking, this means you may need to wait for a symbol to be defined (not just waiting for defined symbols to parse) This is especially bad for function overloading


## TODO - little tasks:

- [ ] "array references" -> array slice
- [ ] unify Proc/Procedure naming - everything should be proc (I think it's just TypeInfoProcedure)
- [x] fix error recovery stack to synch with multiple error outcomes ( ie. failure to parse expression list should recover to ',' or recover to '}'
- [ ] track nested constructs in error recovery?
- [ ] nested block comments
- [ ] Get rid of errep passing, (just add a hide error count to ErrorManager?)

### TODO - Big Stuff:
- [ ] add module support
