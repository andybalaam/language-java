module Main where

import Test.Tasty(defaultMain, testGroup)
import Test.Tasty.HUnit(assertEqual, testCase)

import Control.Applicative
import Language.Java.Parser(parser, compilationUnit)
import Language.Java.Syntax(CompilationUnit)
import Language.Java.Pretty(pretty)
import Text.Parsec.Error(ParseError)


main = defaultMain classicTests


classicTests = testGroup "Classic language-java formatting"

  [ v "Minimalist class" $ unlines
    [ "class X"
    , "{"
    , "}"
    ]

  , v "Public class decl" $ unlines
    [ "public class X"
    , "{"
    , "}"
    ]

  , v "Simple field" $ unlines
    [ "class X"
    , "{"
    , "  int x;"
    , "}"
    ]

  , v "Simple method" $ unlines
    [ "class X"
    , "{"
    , "  void x ()"
    , "  {"
    , "  }"
    , "}"
    ]

  , v "Exception specification" $ unlines
    [ "class X"
    , "{"
    , "  void x () throws Exception"
    , "  {"
    , "  }"
    , "}"
    ]

  ]
  where v = verifyUnchangedByRoundtrip


parse :: String -> [Either ParseError CompilationUnit]
parse code = parser compilationUnit <$> [code]


stripTrailingNewline :: String -> String
stripTrailingNewline x =
    if last x == '\n' then
        init x
    else
        x


verifyUnchangedByRoundtrip desc code =
    let
        cod = stripTrailingNewline code
    in
        testCase ("Roundtrip " ++ desc) $ case head (parse cod) of
            Left  err -> fail (show err)
            Right ast -> assertEqual [] cod $ show $ pretty ast
