{-# LANGUAGE OverloadedStrings #-}
module Test.Parser (parserSpecs) where

import Test.Hspec
import Halcyon.Frontend.Parse (parseTokens)
import Halcyon.Frontend.Tokens
import Halcyon.Core.Ast

parserSpecs :: Spec
parserSpecs = describe "Parser" $ do
  it "parses minimal program" $
    parseTokens [TokInt, TokIdent "main", TokLParen, TokVoid, TokRParen, 
                TokLBrace, TokReturn, TokNumber 42, TokSemicolon, TokRBrace]
    `shouldBe` Right (Program (Function "main" (Return (Constant 42))))

  it "parses unary negation" $
    parseTokens [TokInt, TokIdent "main", TokLParen, TokVoid, TokRParen,
                TokLBrace, TokReturn, TokHyphen, TokNumber 42, TokSemicolon, TokRBrace]
    `shouldBe` Right (Program (Function "main" (Return (Unary Negate (Constant 42)))))