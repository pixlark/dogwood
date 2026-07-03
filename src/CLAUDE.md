### `dogwood-core`

This package holds all of the core functionality of the compiler.

The current state of the compiler is divided into the following phases:

 - the `Lexer` phase, which tokenizes the input stream
    - Located in `Lexer/Internal.hs`
 - the `Parser` phase, which parses the token stream into an abstract syntax tree
    - Located in `Parser/Internal.hs`
    - AST is defined in `AST.hs`
 - the `LowerPass` phase, which lowers syntactical constructs in the AST into simpler constructs in the LoweredAST
    - Located in `LowerPass.hs`
    - LoweredAST is defined in `LoweredAST.hs`
 - the `Typechecker` phase, which typechecks the AST and decorates it with deduced type information
    - Located in `Typechecker/Internal.hs`
    - The type-decorated AST is defined in `TypedAST.hs`
 - The `LoopPass` phase, which checks that loop constructs are defined correctly
    - Located in `LoopPass.hs`
 - The `Compiler` phase, which transforms the typed AST into a static single assignment intermediate representation.
    - Located in `Compiler.hs`
    - The IR is defined in `IR.hs`
 - The `EmitC` phase, which turns the IR into generated C code.
    - Located in `EmitC.hs`
 - The `Clang` phase, which invokes `clang` to compile the generated C code.
    - Located in `Clang.hs`

All these phases are orchestrated and piped into each other in `Frontend.hs`.

#### `Compiler` phase

The SSA transformation is modeled after the algorithm described in "Simple and Efficient Construction of Static Single Assignment Form" by Matthias Braun et al.

If needed for reference, there is a PDF of this paper in the `misc` directory in the project root.

#### Error handling

`Error.hs` defines a few types associated with error handling:

 - The `ErrorKind` sum type lists all the different kinds of errors that the compiler can produce.
 - The `Span` type is simply a range that refers to a location in the original source code.
 - The `Err` type wraps `ErrorKind` and `Span` to associate an error with its origin in the source code.

`Span`s are located on every representation type, and are threaded through every phase of the compiler, so that even in a later phase, we can associate an error with some section of the original source code for the user to reference.
