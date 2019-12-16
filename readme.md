
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
* EWC macros/namespaces changed go MOE
* No more pCoz/pChz, all string code should handle UTF8

## Things I contemplated changing, but didn't
* Alloc -> not using arena based loading - this would be nicer, but I don't think it will work with caching query results

## Actual language changes
* Removing acast 
* Removing for_each
* Using unnamed types require a `:` before the type


## TODO - little tasks:

- [ ] "array references" -> array slice
- [ ] unify Proc/Procedure naming - everything should be proc (I think it's just TypeInfoProcedure)
- [x] fix error recovery stack to synch with multiple error outcomes ( ie. failure to parse expression list should recover to ',' or recover to '}'
- [ ] track nested constructs in error recovery?
- [ ] nested block comments


### TODO - Big Stuff:
- [ ] add module support
