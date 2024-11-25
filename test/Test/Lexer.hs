{-# LANGUAGE OverloadedStrings #-}
module Test.Lexer (lexerSpecs) where

import Test.Hspec
import Text.Megaparsec (parse)
import Halcyon.Frontend.Lexer (lexer)
import Halcyon.Frontend.Tokens

lexerSpecs :: Spec 
lexerSpecs = describe "Lexer" $ do
  describe "basic tokens" $ do
    it "lexes empty input" $
      parse lexer "" "" `shouldParse` []
      
    it "lexes simple return" $
      parse lexer "" "return 42;" `shouldParse` 
        [TokReturn, TokNumber 42, TokSemicolon]

  describe "unary operators" $ do
    it "lexes negation" $
      parse lexer "" "-42" `shouldParse`
        [TokHyphen, TokNumber 42]
        
    it "lexes complement" $
      parse lexer "" "~42" `shouldParse`
        [TokTilde, TokNumber 42]


-- Helper for cleaner parse result assertions  
shouldParse :: (Eq a, Show a, Show e) => Either e a -> a -> Expectation
shouldParse (Right got) expected = got `shouldBe` expected
shouldParse (Left err) _ = expectationFailure $ show err