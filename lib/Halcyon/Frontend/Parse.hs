{-# LANGUAGE OverloadedStrings #-}
module Halcyon.Frontend.Parse 
  ( parseTokens
  , HalcyonParseError
  , ParseResult
  ) where

import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Text (Text)
import qualified Data.Set as Set
import Data.List.NonEmpty (NonEmpty(..))
import Text.Megaparsec
import Text.Megaparsec.Debug (dbg)

import Halcyon.Frontend.Tokens
import Halcyon.Core.Ast
    ( Program(..), Expr(..), Function(Function), Statement(..), UnaryOp(..) )

-- Type aliases to make signatures cleaner
type Parser = Parsec CParseError [CToken]
type HalcyonParseError = ParseErrorBundle [CToken] CParseError
type ParseResult = Either HalcyonParseError Program

-- Main entry point
parseTokens :: [CToken] -> ParseResult
parseTokens ctokens = runParser (parseProgram <* eof) "" ctokens

-- Program level parser
parseProgram :: Parser Program
parseProgram = Program <$> parseFunctionDef <?> "program"

-- Function definition parsers
parseFunctionDef :: Parser Function
parseFunctionDef = do
  name <- parseFunctionHeader
  body <- parseFunctionBody
  return $ Function name body

parseFunctionHeader :: Parser Text
parseFunctionHeader = do
  void $ matchToken KwInt
  name <- identifier
  void $ parseParams
  return name <?> "function header"

parseParams :: Parser ()
parseParams = between 
  (matchToken LParen) 
  (matchToken RParen)
  (void (matchToken KwVoid <?> "void")) <?> "function parameters"

parseFunctionBody :: Parser Statement
parseFunctionBody = between 
  (matchToken LBrace)
  (matchToken RBrace)
  parseStatement <?> "function body"

-- Statement parsers
parseStatement :: Parser Statement
parseStatement = choice
  [ parseReturn
  ] <?> "statement"

parseReturn :: Parser Statement
parseReturn = do
  void (matchToken KwReturn <?> "return keyword")
  expr <- parseExpr
  void (matchToken Semicolon <?> "semicolon")
  return $ Return expr

-- Expression parsers
parseExpr :: Parser Expr
parseExpr = dbg "expr" $ parseTerm

parseTerm :: Parser Expr
parseTerm = dbg "term" $ choice
  [ parens parseExpr
  , parseUnary
  , Constant <$> number
  ]

parens :: Parser a -> Parser a
parens = between
  (matchToken LParen)
  (matchToken RParen)

parseUnary :: Parser Expr
parseUnary = dbg "unary" $ do
  op <- pUnaryOp
  expr <- parseTerm
  case op of
    Tilde -> 
      pure $ Unary Complement expr
    Hyphen -> 
      pure $ Unary Negate expr
    DoubleHyphen -> 
      customFailure $ UnexpectedToken DoubleHyphen "Not yet implemented"
    badTok -> 
      customFailure $ UnexpectedToken badTok "No."

pUnaryOp :: Parser CToken
pUnaryOp = (matchToken Tilde <?> "bitwise complement operator")
  <|> (matchToken Hyphen <?> "negation operator")
  <|> (matchToken DoubleHyphen <?> "decrement operator")

-- Token level parsers
matchToken :: CToken -> Parser CToken
matchToken expected = token test expectedSet <?> show expected
  where
    test x = if x == expected 
             then Just x 
             else Nothing
    expectedSet = Set.singleton $ Tokens (expected :| [])

identifier :: Parser Text
identifier = token test expectedSet <?> "identifier"
  where
    test (Identifier t) = Just t
    test _ = Nothing
    expectedSet = Set.singleton $ Label ('i' :| "dentifier")

number :: Parser Int
number = token test expectedSet <?> "number"
  where
    test (Number n) = Just n
    test _ = Nothing
    expectedSet = Set.singleton $ Label ('n' :| "umber")
