module Main where

import Test.Tasty(defaultMain, testGroup)
import Test.Tasty.HUnit(assertEqual, testCase)

import Control.Applicative
import Language.Java.Parser(parser, compilationUnit)
import Language.Java.Syntax(CompilationUnit)
import Language.Java.Pretty(pretty)
import Text.Parsec.Error(ParseError)


main = defaultMain unitTests


unitTests = testGroup "Unit tests"
  [ v "Minimalist class" "class X\n{\n}"
  , v "Public class decl" "public class X\n{\n}"
  , v "Simple field" "public class X\n{\n  int x;\n}"
  ]
  where v = verifyUnchangedByRoundtrip


parse :: String -> [Either ParseError CompilationUnit]
parse code = parser compilationUnit <$> [code]


verifyUnchangedByRoundtrip desc code =
    testCase ("Roundtrip " ++ desc) $ case head (parse code) of
        Left e -> fail (show e)
        Right c -> assertEqual [] code $ show $ pretty c
