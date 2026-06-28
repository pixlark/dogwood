{-# LANGUAGE QuasiQuotes #-}

module CompilerSpec (spec) where

import Common
import Compiler
import Data.Text (append, show)
import Effectful (runEff)
import IR
import LexicalScopes (mkScopes)
import Logging (noOpLogger, runLog)
import NeatInterpolation
import Test.Hspec
import TypedAST
import Prelude hiding (show)

-- -- | Run the compiler with logging disabled for tests
-- testRunCompiler :: Text -> Result Program
-- testRunCompiler = runEff . runLog noOpLogger . runCompiler

-- expectText text = (`append` "\n") <$> Right text

spec =
  describe "the Compiler module" do
    return undefined

-- spec =
--   describe "the Compiler module" do
--     it "can emit instructions properly" do
--       let compiler = mkCompiler
--       let result = runPureEff $ runErrorNoCallStack $ execState compiler do
--             emit (makeValueExpr Int) (RInt 5) undefined
--       result
--         `shouldBe` Right
--           ( Compiler
--               { nameCounter = 1,
--                 blockCounter = 1,
--                 varCounter = 0,
--                 program = Program [(BlockId 0, Block [SSA (makeValueExpr Int) (Name 0) (RInt 5)] Halt)],
--                 currentBlock = BlockId 0,
--                 scopes = mkScopes,
--                 variables = [],
--                 sealed = [],
--                 currentBreakBlocks = []
--               }
--           )
--       show . program <$> result `shouldBe` Right "__0:\n  _0: int = 5\n  halt\n"
--     it "can compile basic expressions" do
--       show <$> testRunCompiler "5 + 6"
--         `shouldBe` expectText
--           [text|
--             __0:
--               _0: int = 5
--               _1: int = 6
--               _2: int = _0 + _1
--               halt
--           |]
--       show <$> testRunCompiler "5 * -7"
--         `shouldBe` expectText
--           [text|
--             __0:
--               _0: int = 5
--               _1: int = 7
--               _2: int = -_1
--               _3: int = _0 * _2
--               halt
--           |]
--       show <$> testRunCompiler "{ 5; 7; }"
--         `shouldBe` expectText
--           [text|
--             __0:
--               _0: int = 5
--               _1: int = 7
--               _2: void = void
--               halt
--           |]
--     it "can handle variables" do
--       show <$> testRunCompiler "{ let x: int = 5; x + 3 }"
--         `shouldBe` expectText
--           [text|
--             __0:
--               _0: int = 5
--               _1: int = 3
--               _2: int = _0 + _1
--               halt
--           |]
--     it "can compile loops" do
--       show <$> testRunCompiler "{ let x: int = 5; loop { x + 2; } 6 }"
--         `shouldBe` expectText
--           [text|
--             __0:
--               _0: int = 5
--               jump __1
--             __1:
--               _1: int = 2
--               _2: int = _0 + _1
--               _3: void = void
--               jump __1
--             __2:
--               _4: int = 6
--               halt
--           |]
--       show <$> testRunCompiler "{ loop { break; } }"
--         `shouldBe` expectText
--           [text|
--             __0:
--               jump __1
--             __1:
--               jump __2
--             __3:
--               _0: void = void
--               jump __1
--             __2:
--               _1: void = void
--               halt
--           |]
--     it "can compile if expressions" do
--       show <$> testRunCompiler "{ if true 5 else 6 }"
--         `shouldBe` expectText
--           [text|
--             __0:
--               jump __1
--             __1:
--               _0: bool = true
--               jump if _0 to __4 else __2
--             __4:
--               _1: int = 5
--               jump __3
--             __2:
--               _2: int = 6
--               jump __3
--             __3:
--               _3: int = phi __4[_1], __2[_2]
--               halt
--           |]
--       show <$> testRunCompiler "{ if true 5 else if false 6 else 7 }"
--         `shouldBe` expectText
--           [text|
--             __0:
--               jump __1
--             __1:
--               _0: bool = true
--               jump if _0 to __5 else __2
--             __5:
--               _1: int = 5
--               jump __4
--             __2:
--               _2: bool = false
--               jump if _2 to __6 else __3
--             __6:
--               _3: int = 6
--               jump __4
--             __3:
--               _4: int = 7
--               jump __4
--             __4:
--               _5: int = phi __5[_1], __6[_3], __3[_4]
--               halt
--           |]
--       show <$> testRunCompiler "if true { 5; }"
--         `shouldBe` expectText
--           [text|
--             __0:
--               jump __1
--             __1:
--               _0: bool = true
--               jump if _0 to __4 else __2
--             __4:
--               _1: int = 5
--               _2: void = void
--               jump __3
--             __2:
--               _3: void = void
--               jump __3
--             __3:
--               _4: void = phi __4[_2], __2[_3]
--               halt
--           |]
--     describe "can compile complex programs" do
--       it "can compile a factorial program" do
--         show
--           <$> testRunCompiler
--             [text|
--               {
--                 let n: int = 5;
--                 let acc: int = 1;
--                 loop {
--                   if n == 0 {
--                     break;
--                   }
--                   acc = acc * n;
--                   n = n - 1;
--                 }
--               }
--             |]
--           `shouldBe` expectText
--             undefined
