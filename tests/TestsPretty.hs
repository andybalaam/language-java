module Main where

import Test.Tasty
import Test.Tasty.HUnit

--import Language.Java.Parser
--import Language.Java.Syntax
--import Language.Java.Pretty

main = defaultMain unitTests


unitTests = testGroup "Unit tests"
  [ testCase "Sanity" $ assertEqual [] (1 :: Integer) 1
  ]

