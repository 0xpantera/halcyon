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
    ( Program(..), Expr(..), FunctionDef(Function), Statement(..), UnaryOp(..) )

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
parseFunctionDef :: Parser FunctionDef
parseFunctionDef = do
  name <- parseFunctionHeader
  body <- parseFunctionBody
  return $ Function name body

parseFunctionHeader :: Parser Text
parseFunctionHeader = do
  void $ matchToken TokInt
  name <- identifier
  void $ parseParams
  return name <?> "function header"

parseParams :: Parser ()
parseParams = between 
  (matchToken TokLParen) 
  (matchToken TokRParen)
  (void (matchToken TokVoid <?> "void")) <?> "function parameters"

parseFunctionBody :: Parser Statement
parseFunctionBody = between 
  (matchToken TokLBrace)
  (matchToken TokRBrace)
  parseStatement <?> "function body"

-- Statement parsers
parseStatement :: Parser Statement
parseStatement = choice
  [ parseReturn
  ] <?> "statement"

parseReturn :: Parser Statement
parseReturn = do
  void (matchToken TokReturn <?> "return keyword")
  expr <- parseExpr
  void (matchToken TokSemicolon <?> "semicolon")
  return $ Return expr

-- Expression parsers
parseExpr :: Parser Expr
parseExpr = dbg "expr" (choice
  [ dbg "unary" parseUnary <?> "unary operator"
  , dbg "term" parseTerm <?> "term"
  ] <?> "expression")

parseTerm :: Parser Expr
parseTerm = dbg "term" (choice
  [ dbg "constant" (Constant <$> number)
  , dbg "paren expr" (parens parseExpr)
  ] <?> "term")

parens :: Parser a -> Parser a
parens = between
  (matchToken TokLParen)
  (matchToken TokRParen)

parseUnary :: Parser Expr
parseUnary = dbg "unary" $ do
  op <- dbg "unaryOp" pUnaryOp
  expr <- dbg "unaryExp" parseExpr
  case op of
    TokTilde -> 
      pure $ Unary Complement expr
    TokHyphen -> 
      pure $ Unary Negate expr
    TokDoubleHyphen -> 
      customFailure $ UnexpectedToken TokDoubleHyphen "Not yet implemented"
    badTok -> 
      customFailure $ UnexpectedToken badTok "No."

pUnaryOp :: Parser CToken
pUnaryOp = (matchToken TokTilde <?> "bitwise complement operator")
  <|> (matchToken TokHyphen <?> "negation operator")
  <|> (matchToken TokDoubleHyphen <?> "decrement operator")

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
    test (TokIdent t) = Just t
    test _ = Nothing
    expectedSet = Set.singleton $ Label ('i' :| "dentifier")

number :: Parser Int
number = token test expectedSet <?> "number"
  where
    test (TokNumber n) = Just n
    test _ = Nothing
    expectedSet = Set.singleton $ Label ('n' :| "umber")
