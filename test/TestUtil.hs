module TestUtil where

import Test.Hspec

shouldSatisfyM action p = action >>= (`shouldSatisfy` p)
