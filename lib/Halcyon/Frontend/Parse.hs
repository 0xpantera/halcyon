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
import qualified Data.List.NonEmpty as NE
import Text.Megaparsec

import Halcyon.Frontend.Tokens
import Halcyon.Core.Ast

-- Type aliases to make signatures cleaner
type Parser = Parsec CParseError [CToken]
type HalcyonParseError = ParseErrorBundle [CToken] CParseError
type ParseResult = Either HalcyonParseError Program

-- Main entry point
parseTokens :: [CToken] -> ParseResult
parseTokens ctokens = runParser (pProgram <* eof) "" ctokens

pProgram :: Parser Program
pProgram = Program <$> pFunction <?> "program"

pFunction :: Parser Function
pFunction = do
  name <- pFunctionHeader
  body <- pFunctionBody
  return $ Function name body

pFunctionHeader :: Parser Text
pFunctionHeader = do
  void $ matchToken KwInt
  name <- identifier
  void $ pParams
  return name <?> "function header"

pParams :: Parser ()
pParams = parens
  (void (matchToken KwVoid <?> "void")) <?> "function parameters"

pFunctionBody :: Parser Statement
pFunctionBody = between 
  (matchToken LBrace)
  (matchToken RBrace)
  pStatement <?> "function body"

pStatement :: Parser Statement
pStatement = choice
  [ pReturn
  ] <?> "statement"

pReturn :: Parser Statement
pReturn = do
  void (matchToken KwReturn <?> "return keyword")
  expr <- pExpr
  void (matchToken Semicolon <?> "semicolon")
  return $ Return expr

pExpr :: Parser Expr  
pExpr = makeExprParser pTerm operatorTable

pTerm :: Parser Expr
pTerm = choice
  [ parens pExpr
  , Constant <$> number
  , pUnary
  ]

pUnary :: Parser Expr
pUnary = try $ do
  op <- choice
    [ Complement <$ symbol Tilde
    , Negate     <$ symbol Hyphen
    ]
  Unary op <$> pTerm

-- Operator table with precedence levels
operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [
    [ InfixL (Binary Multiply   <$ symbol Star)
    , InfixL (Binary Divide     <$ symbol Slash) 
    , InfixL (Binary Remainder  <$ symbol Percent)
    ]
  , [ InfixL (Binary Add        <$ symbol Plus)
    , InfixL (Binary Subtract   <$ symbol Hyphen)
    ]
  ]

parens :: Parser a -> Parser a
parens = between
  (matchToken LParen)
  (matchToken RParen)

-- | Match a token and provide error context 
symbol :: CToken -> Parser CToken
symbol tok = matchToken tok <?> show tok

matchToken :: CToken -> Parser CToken 
matchToken expected = matchWith 
  (\x -> if x == expected then Just x else Nothing)
  (show expected)

number :: Parser Int
number = matchWith (\case 
    Number n -> Just n
    _ -> Nothing) "number"

identifier :: Parser Text
identifier = matchWith (\case
    Identifier t -> Just t
    _ -> Nothing) "identifier"

-- Common abstraction for token matching
matchWith :: (CToken -> Maybe a) -> String -> Parser a
matchWith f tag = 
  token f (Set.singleton $ Label (NE.fromList tag)) <?> tag
