{-# LANGUAGE OverloadedStrings #-}
module Halcyon.Frontend.Parse 
  ( parseTokens
  , HalcyonParseError
  , ParseResult
  ) where

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Set as Set
import Data.List.NonEmpty (NonEmpty(..))
import Text.Megaparsec
import Control.Monad.Combinators.Expr ( Operator )

import Halcyon.Frontend.Tokens ( CToken(..), CTokenParseError ) 
import Halcyon.Core.Ast
    ( Program(..), Expr(..), FunctionDef(Function), Statement(..) )

-- Type aliases to make signatures cleaner
type Parser = Parsec CTokenParseError [CToken]
type HalcyonParseError = ParseErrorBundle [CToken] CTokenParseError
type ParseResult = Either HalcyonParseError Program

-- Main entry point
parseTokens :: [CToken] -> ParseResult
parseTokens ctokens = runParser (parseProgram <* eof) "" ctokens

-- Program level parser
parseProgram :: Parser Program
parseProgram = Program <$> parseFunctionDef

-- Function definition parsers
parseFunctionDef :: Parser FunctionDef
parseFunctionDef = 
  Function <$> parseFunctionHeader <*> parseFunctionBody

parseFunctionHeader :: Parser Text
parseFunctionHeader = do
  void $ matchToken TokInt
  name <- identifier
  void $ parseParams
  pure name

parseParams :: Parser ()
parseParams = void $ between 
  (matchToken TokLParen) 
  (matchToken TokRParen)
  (matchToken TokVoid)

parseFunctionBody :: Parser Statement
parseFunctionBody = between 
  (matchToken TokLBrace)
  (matchToken TokRBrace)
  parseStatement

-- Statement parsers
parseStatement :: Parser Statement
parseStatement = choice
  [ parseReturn
  ] <?> "statement"

parseReturn :: Parser Statement
parseReturn = Return <$> 
  (matchToken TokReturn *> parseExpr <* matchToken TokSemicolon)
  <?> "return statement"

-- Expression parsers
-- Currently only handles constants, but structured for future expansion
parseExpr :: Parser Expr
parseExpr = parseTerm <?> "expression"

parseTerm :: Parser Expr
parseTerm = choice
  [ Constant <$> number
  ] <?> "term"

-- Token level parsers
matchToken :: CToken -> Parser CToken
matchToken expected = token test expectedSet
  where
    test x = if x == expected then Just x else Nothing
    expectedSet = Set.singleton $ Tokens (expected :| [])
    
identifier :: Parser Text
identifier = token test Set.empty <?> "identifier"
  where
    test (TokIdent t) = Just t
    test _ = Nothing

number :: Parser Int
number = token test Set.empty <?> "number"
  where
    test (TokNumber n) = Just n
    test _ = Nothing

-- Prepared for future operator implementation
operatorTable :: [[Operator Parser Expr]]
operatorTable = []