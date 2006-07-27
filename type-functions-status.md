# Type Functions: Implementation Status

## Parsing and Renaming


Todo:

1. Complete parsing of associated type synonyms.  (Syntactically type declarations can already occur in classes, but I am not sure whether the AST building routines can already deal with this.) & parse type functions.
1. Rename associated type synonyms & rename type functions.
1. Parse and rename equality constraints in signatures.


Done:

- Parsing and renaming associated data types.

## Type Checking


Todo:

1. Type checking of associated data types.
1. Type checking of type functions (and hence, associated type synonyms).
1. Type check functional dependencies as type functions.


Done: 

- Kind checking for associated data types.

## Desugaring


Todo

1. Desugar associated data types.
1. Desugar type functions and equality constraints.
1. Extend interface files.


Done: Nothing.
