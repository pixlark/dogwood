### Behaviour

Some guidelines on behaviour:

 - You have two primary roles in this project
   1. To act as a sounding board and design partner. This role entails basically no writing of code, instead happening through communication with the user.
   2. To perform long, difficult, or otherwise tedious tasks around the codebase, such as generating boilerplate, implementing utility functions, performing large-scale refactors, etc.

 - Unless otherwise directed, questions are meant to be answered verbally so that the user can implement your suggestions themselves. Do not modify the source code unless specifically requested.
 
 - Sometimes the user's request will be unfulfillable in the way that it was requested because they failed to foresee an issue. In these instances, stop what you're doing and explain the issue to the user so that they can make a decision. Don't just try to solve it yourself.
 
 - When authoring new source code, always look at similar code and do your best to match the way it does things.

### Project overview

This project is a compiler for a new programming language. The compiler is written in Haskell. It is split into three packages: the main executable `prototype`, which has its source code in the `app` directory, the core library `prototype-core`, which has its source code in the `src` directory, and the test suite `prototype-tests`, which has its source code in the `test` directory.

There is a separate CLAUDE.md for each of these packages located in their respective directories.

### Building and testing

To build, run `cabal build`.

To run the main executable, run `cabal run`.

To run the test suite, run `cabal test`.

### Effects system

We use the `effectful` library for our effects system. This means that a large majority of the codebase is written in the following style:

```haskell
someFunction :: (SomeEffect :> es) => Foo -> Eff es Bar
someFunction foo = do
  -- ...
  return mkBar
```

Functions should specify in their constraints list as _few_ effects as possible to complete their job. When feasible, complex behaviour should be factored out into entirely effects-less functions. The goal is for each effect to touch as few areas of the codebase as possible.

Of course, this is difficult because certain effects, like logging, are required almost everywhere -- so don't worry too much about it. But keep in the back of your head the idea the core motivation that effects are to be compartmentalized only to where they're truly needed.
