### `runtime`

This directory contains the runtime implementation for the language.

It is compiled as a shared library (`libruntime.so`), and linked in to compiled programs.

#### Builtin functions

Builtin functions (declared with the `builtin` keyword in the language) have their actual implementations here (prefixed with `builtin_`).

#### Value boxing

The `any` type allows the user to have variables that can contain any type of value. In the runtime, this is implemented by boxing values when they get coerced to the `any` type.

Boxing values involves allocating space on the heap for them, and attaching runtime type information, so that the value can be safely unboxed later.

#### Garbage collector

The runtime also contains an implementation of a shadow-stack-based garbage collector. Right now this isn't enabled.
