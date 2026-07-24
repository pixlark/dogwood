### `dogwood-tests`

This package uses the `Hspec` library to test each phase of the compiler.

Each phase of the compiler has its own test specification file. For example, the `Lexer` phase is tested in `LexerSpec.hs`.

The `IntegrationSpec.hs` tests run the entire compiler chain on input source files and match them against an expected output. These source files are in the `integration_tests` subdirectory.

#### Guidelines for new tests

 - The most important rule is to avoid writing superflous tests. If the behavior being tested is simple, then it doesn't need a test. The more tests there are, the more noise and the more breakage there is when changes are made to the compiler. So when you add new tests, make sure they're testing something that is actually complex in the implementation.
 - In a similar vein, make sure that edge-case tests are actual edge cases in the implementation. For example, if you have a "set variable" test, you don't also need a "set two variables" test, or a "set variable inside of a function" test, because although those are technically variants, they don't actually stress-test any important details of the implementation -- they are trivially true as long as the original "set variable" test is true.
 - Tests should be relatively self-documenting. Avoid scattering comments everywhere - the name of the test should describe exactly what it does. Only add comments if the test is particularly complex.
