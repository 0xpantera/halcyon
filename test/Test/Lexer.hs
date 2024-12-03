{-# LANGUAGE OverloadedStrings #-}
module Test.Lexer (lexerSpecs) where

import Test.Hspec
import Text.Megaparsec (parse)
import Halcyon.Frontend (lexer, CToken(..))

lexerSpecs :: Spec 
lexerSpecs = describe "Lexer" $ do
  describe "basic tokens" $ do
    it "lexes empty input" $
      parse lexer "" "" `shouldParse` []
      
    it "lexes simple return" $
      parse lexer "" "return 42;" `shouldParse` 
        [KwReturn, Number 42, Semicolon]

  describe "unary operators" $ do
    it "lexes negation" $
      parse lexer "" "-42" `shouldParse`
        [Hyphen, Number 42]
        
    it "lexes complement" $
      parse lexer "" "~42" `shouldParse`
        [Tilde, Number 42]

    it "lexes binary operators" $
      parse lexer "" "1 + 2 * 3" `shouldParse`
        [Number 1, Plus, Number 2, Star, Number 3]


-- Helper for cleaner parse result assertions  
shouldParse :: (Eq a, Show a, Show e) => Either e a -> a -> Expectation
shouldParse (Right got) expected = got `shouldBe` expected
shouldParse (Left err) _ = expectationFailure $ show err