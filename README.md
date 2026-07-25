### The `dogwood` language

`dogwood` is a programming language in very early development.

Its main goals are to have:
 - Fully automatic memory management through garbage collection
 - Robust polymorphism with multi-parameter typeclasses
 - Language server integrated into the compiler
 - Strong C interop
 - Algebraic data-types with pattern matching
 - Lightweight green-threads

The syntax of the language looks very similar to Rust, which is on purpose. This language was conceived as a slightly higher-level alternative to Rust that eschews its manual memory management but keeps all the other nice things that people love about it.

Here is a code sample (this doesn't quite compile yet, but most of it is there):

```
fn main() {
    print(factorial(10));
}

fn factorial(n: int) -> int {
    if n == 0 {
        1
    } else {
        n * factorial(n - 1)
    }
}
```

The name "dogwood" is temporary, by the way, it's just a random name picked by leafing through a book.

#### Statement on LLM use

This project makes minimal use of LLMs for programming assistance. All code from LLMs is manually reviewed and understood by a human before being committed, and generally gets significantly modified anyways.

Use of LLMs in general tends to be limited to speeding up routine tasks such as generating boilerplate and refactoring, or as a rubber duck when thinking about design or tracking down bugs. However, they _are_ used to some extent, so if you have ethical objections to LLM use, you may want to look elsewhere.
