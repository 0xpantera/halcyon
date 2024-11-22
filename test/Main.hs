{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Test.Tasty
import Test.Tasty.Hspec

import qualified Test.Lexer as Lex
import qualified Test.Parser as Parse
import qualified Test.Tacky as Tacky
import qualified Test.Assembly as Asm

main :: IO ()
main = do
  specs <- concat <$> mapM testSpecs
    [ Lex.lexerSpecs
    , Parse.parserSpecs
    , Tacky.tackySpecs  
    , Asm.assemblySpecs
    ]
  defaultMain (testGroup "All Tests" [
      testGroup "Specs" specs
      -- Add property/golden tests later:
      -- , testGroup "Properties" props
      -- , testGroup "Golden Tests" goldens
      ])