{-# LANGUAGE OverloadedStrings #-}
module Parse where

import Control.Monad (void)
import Data.Void
import Data.Text (Text)
import Text.Megaparsec

import Tokens
import Ast

-- Specialized parser type for working with tokens
type Parser = Parsec Void [CToken]

-- Custom token parser
tokenParser :: (CToken -> Maybe a) -> Parser a
tokenParser test = token' where
  token' = do
    cTokens <- getInput
    case cTokens of
      [] -> fail "unexpected end of input"
      (t:ts) -> case test t of
        Nothing -> fail $ "unexpected token: " ++ show t
        Just x -> do
          setInput ts
          return x

-- Match a specific token
matchToken :: CToken -> Parser CToken
matchToken t = tokenParser test
  where
    test x = if x == t then Just x else Nothing

-- Parse an identifier token and extract the name
identifier :: Parser Text
identifier = tokenParser test
  where
    test (TokIdent t) = Just t
    test _ = Nothing

-- Parse a number token and extract the value
number :: Parser Int
number = tokenParser test
  where
    test (TokNumber n) = Just n
    test _ = Nothing

-- Parse an expression (currently only constants)
parseExpr :: Parser Expr
parseExpr = Constant <$> number

-- Parse a return statement
parseStatement :: Parser Statement
parseStatement = do
  void $ matchToken TokReturn
  expr <- parseExpr
  void $ matchToken TokSemicolon
  return $ Return expr

-- Parse a function definition
parseFunctionDef :: Parser FunctionDef
parseFunctionDef = do
  void $ matchToken TokInt
  name <- identifier
  void $ matchToken TokLParen
  void $ matchToken TokVoid
  void $ matchToken TokRParen
  void $ matchToken TokLBrace
  body <- parseStatement
  void $ matchToken TokRBrace
  return $ Function name body

-- Parse a complete program
parseProgram :: Parser Program
parseProgram = Program <$> parseFunctionDef

-- Main entry point
parse :: [CToken] -> Either (ParseErrorBundle [CToken] Void) Program
parse cTokens = runParser (parseProgram <* eof) "" cTokens