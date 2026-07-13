---
name: rename
description: Rename a symbol across the entire project
allowed-tools: Edit(app/**), Edit(src/**), Edit(test/**), Read, Glob, Grep
---

# Rename skill

This skill instructs you to rename a symbol in the project to something else. This could be a type name, a constructor name, a function name, or even a locally-scoped identifier. The argument to the skill will specify exactly what symbol is to be renamed. You should go through all the packages, find every use of that symbol, and rename each use.

Furthermore, if the symbol you're renaming is a type name, then you should also rename variables of that type, if the variable name is named after the type. For example, if you are renaming the type `data Foo = ...` to `Bar`, then the following function:

```haskell
test :: Foo -> Int
test foo = ...
```

should be renamed like this:

```haskell
test :: Bar -> Int
test bar = ...
```

However, if the variable isn't named after the type:

```haskell
test :: Foo -> Int
test x = ...
```

Then that symbol _shouldn't_ be renamed:

```haskell
test :: Bar -> Int
test x = ...
```

When renaming variable identifiers, don't shorten them unless instructed explicitly to do so. For example, if you're renaming `Foo` to `Bar` again, and you encounter a variable of type `Foo` named `foo`, don't rename it to `b`, always rename it to the full `bar`.

## Instructions

What follows is the description of the rename that was passed to this skill invocation:

$ARGUMENTS

Now, please execute the rename based on that description. If the description isn't specific enough, stop and ask the user to clarify.
