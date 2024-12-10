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
import Halcyon.Core.Ast qualified as Ast

-- Type aliases to make signatures cleaner
type Parser = Parsec CParseError [CToken]
type HalcyonParseError = ParseErrorBundle [CToken] CParseError
type ParseResult = Either HalcyonParseError Ast.Program

-- Main entry point
parseTokens :: [CToken] -> ParseResult
parseTokens ctokens = runParser (pProgram <* eof) "" ctokens

pProgram :: Parser Ast.Program
pProgram = Ast.Program <$> pFunction <?> "program"

pFunction :: Parser Ast.Function
pFunction = do
  name <- pFunctionHeader
  body <- pFunctionBody
  return $ Ast.Function name body

pFunctionHeader :: Parser Text
pFunctionHeader = do
  void $ matchToken KwInt
  name <- identifier
  void $ pParams
  return name <?> "function header"

pParams :: Parser ()
pParams = parens
  (void (matchToken KwVoid <?> "void")) <?> "function parameters"

pFunctionBody :: Parser Ast.Statement
pFunctionBody = between 
  (matchToken LBrace)
  (matchToken RBrace)
  pStatement <?> "function body"

pStatement :: Parser Ast.Statement
pStatement = choice
  [ pReturn
  ] <?> "statement"

pReturn :: Parser Ast.Statement
pReturn = do
  void (matchToken KwReturn <?> "return keyword")
  expr <- pExpr
  void (matchToken Semicolon <?> "semicolon")
  return $ Ast.Return expr

pExpr :: Parser Ast.Expr  
pExpr = makeExprParser pTerm operatorTable

pTerm :: Parser Ast.Expr
pTerm = choice
  [ parens pExpr
  , Ast.Constant <$> number
  , pUnary
  ]

pUnary :: Parser Ast.Expr
pUnary = try $ do
  op <- choice
    [ Ast.Complement <$ symbol Tilde
    , Ast.Negate     <$ symbol Hyphen
    , Ast.Not        <$ symbol Bang
    ]
  Ast.Unary op <$> pTerm


{-
These operators have lower
precedence than the ones before, and they’re all left-associative.
Among the new operators, <, <=, >, and >= have the highest precedence,
followed by the equality operators, == and !=. The && operator has lower
precedence than the equality operators, and || has the lowest precedence
of all. 
-}

-- Operator table with precedence levels
operatorTable :: [[Operator Parser Ast.Expr]]
operatorTable =
  [
    [ InfixL (Ast.Binary Ast.Multiply    <$ symbol Star)
    , InfixL (Ast.Binary Ast.Divide      <$ symbol Slash) 
    , InfixL (Ast.Binary Ast.Remainder   <$ symbol Percent)
    ]
  , [ InfixL (Ast.Binary Ast.Add         <$ symbol Plus)
    , InfixL (Ast.Binary Ast.Subtract    <$ symbol Hyphen)
    ]
  , [ InfixL (Ast.Binary Ast.LessThan    <$ symbol LessThan)
    , InfixL (Ast.Binary Ast.LessOrEqual <$ symbol LessOrEqual)
    , InfixL (Ast.Binary Ast.GreaterThan <$ symbol GreaterOrEqual)
    , InfixL (Ast.Binary Ast.GreaterOrEqual <$ symbol GreaterOrEqual)
    ]
  , [ InfixL (Ast.Binary Ast.Equal <$ symbol DoubleEqual)
    , InfixL (Ast.Binary Ast.NotEqual <$ symbol NotEqual)
    ]
  , [ InfixL (Ast.Binary Ast.And <$ symbol LogicalAnd)
    ]
  , [ InfixL (Ast.Binary Ast.Or <$ symbol LogicalOr)
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
