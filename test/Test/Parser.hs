{-# LANGUAGE OverloadedStrings #-}
module Test.Parser (parserSpecs) where

import Test.Hspec
import Halcyon.Frontend (parseTokens, CToken(..))
import Halcyon.Core.Ast

parserSpecs :: Spec
parserSpecs = describe "Parser" $ do
  it "parses minimal program" $
    parseTokens [KwInt, Identifier "main", LParen, KwVoid, RParen, 
                LBrace, KwReturn, Number 42, Semicolon, RBrace]
    `shouldBe` Right (Program (Function "main" (Return (Constant 42))))

  it "parses unary negation" $
    parseTokens [KwInt, Identifier "main", LParen, KwVoid, RParen,
                LBrace, KwReturn, Hyphen, Number 42, Semicolon, RBrace]
    `shouldBe` Right (Program (Function "main" (Return (Unary Negate (Constant 42)))))