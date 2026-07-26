{-# LANGUAGE BangPatterns #-}

module Main where

import DW.Common
import DW.Frontend

import Criterion.Main
import Criterion.Types
import Data.Foldable (foldl')
import Data.Text qualified as T
import System.FilePath (FilePath)
import Text.Printf

criterionBenchmarks :: (HasCallStack) => IO ()
criterionBenchmarks =
  defaultMain
    [ mkBench "one thousand bindings and assigns" (return bench1),
      mkBench "one hundred functions" (do putStrLn $ T.unpack bench2; return bench2)
    ]
  where
    bench1 =
      let lines = take 1000 $ cycle ["let x = 15;\n", "x = 12;\n"]
          lines' = ["let main = fn() {\n"] ++ lines ++ ["};\n"]
       in foldl' T.append "" lines'
    bench2 =
      let iters = 100
          lines = take iters $
            flip map [1 :: Int ..] $
              \i -> T.pack $ printf "let fn%d = fn(x: int) -> int { if x == 2 { fn%d(x + 1) } else { fn%d(x - 1) } };" i (i - 1) (i - 1)
          lines' = lines ++ ["let fn0 = fn(x: int) -> int: x;", T.pack $ printf "let main = fn() { let print = builtin print; print(fn%d(10)); };" iters]
       in foldl' T.append "" (map (`T.append` "\n") lines')

mkBench :: String -> IO Text -> Benchmark
mkBench name io = env io $ \src -> bench name $ nfIO (runBench src)

main :: IO ()
main = do
  criterionBenchmarks
