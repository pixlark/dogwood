### `dogwood-core`

This package holds all of the core functionality of the compiler.

The current state of the compiler is divided into the following phases:

 - the `Lexer` phase, which tokenizes the input stream
    - Located in `DW/Lexer/Internal.hs`
 - the `Parser` phase, which parses the token stream into an abstract syntax tree
    - Located in `DW/Parser/Internal.hs`
    - AST is defined in `AST.hs`
 - the `ConstExprPass` phase, which ensures that top-level initializers are const-compatible.
    - Located in `DW/ConstExprPass.hs`
 - the `LowerPass` phase, which lowers syntactical constructs in the AST into simpler constructs in the LoweredAST
    - Located in `DW/LowerPass.hs`
    - LoweredAST is defined in `LoweredAST.hs`
 - the `Typechecker` phase, which typechecks the AST and decorates it with deduced type information
    - Located in `DW/Typechecker/Internal.hs`
    - The type-decorated AST is defined in `TypedAST.hs`
 - The `LoopPass` phase, which checks that loop constructs are defined correctly
    - Located in `DW/LoopPass.hs`
 - The `Compiler` phase, which transforms the typed AST into a static single assignment intermediate representation.
    - Located in `DW/Compiler.hs`
    - The IR is defined in `IR.hs`
 - The `EmitC` phase, which turns the IR into generated C code.
    - Located in `DW/EmitC.hs`
 - The `Clang` phase, which invokes `clang` to compile the generated C code.
    - Located in `DW/Clang.hs`

All these phases are orchestrated and piped into each other in `Frontend.hs`.

#### `Compiler` phase

The SSA transformation is modeled after the algorithm described in "Simple and Efficient Construction of Static Single Assignment Form" by Matthias Braun et al.

If needed for reference, there is a PDF of this paper in the `misc` directory in the project root.

### LSP support

LSP support is built into the compiler and developed in tandem. This is to make sure that new users have out-of-the-box IDE support, and that when new language versions are release, the language server never falls behind.

The LSP is implemented in `DW/LSP.hs`, using the same `lsp` package that powers Haskell Language Server.

#### Error handling

`Error.hs` defines a few types associated with error handling:

 - The `ErrorKind` sum type lists all the different kinds of errors that the compiler can produce.
 - The `Span` type is simply a range that refers to a location in the original source code.
 - The `Err` type wraps `ErrorKind` and `Span` to associate an error with its origin in the source code.
 - The `Errors` effect is used instead of effectful's `Error` effect. It adds the ability to mark down errors without aborting the computation immediately.

`Span`s are located on every representation type, and are threaded through each phase, so that even in a later phase, we can associate an error with some section of the original source code for the user to reference.

For implementation simplicity, internal compiler errors aren't threaded through the `Errors` effect. Instead, they are raised as normal exceptions, with no attached `Span`.

Because of this, from the `Compiler` phase and onwards, the `Errors` effect isn't needed at all, because by that point we should be certain that there are no user errors -- the only errors that could be raised are issues with the compiler itself.
