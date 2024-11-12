{-# LANGUAGE OverloadedStrings #-}
module Lexer where

import Data.Functor (($>))
import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Text as T

import Tokens

-- Parser type
type Parser = Parsec Void Text

-- Space consumer that handles newlines (used between tokens)
scn :: Parser ()
scn = L.space
  space1  -- handles spaces, tabs, and newlines
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme scn

symbol :: Text -> Parser Text
symbol = L.symbol scn

-- Individual token parsers
pKeyword :: Text -> Parser CToken
pKeyword kw = lexeme $ try $ do
  _ <- string kw
  notFollowedBy alphaNumChar
  return $ case kw of
    "int"    -> TokInt
    "void"   -> TokVoid
    "return" -> TokReturn
    _        -> error "Invalid keyword"

pIdentifier :: Parser CToken
pIdentifier = lexeme $ try $ do
  first <- letterChar <|> char '_'
  rest <- many (alphaNumChar <|> char '_')
  let ident = T.pack (first:rest)
  return $ TokIdent ident

pNumber :: Parser CToken
pNumber = lexeme $ try $ do
  digits <- some digitChar
  followedByLetter <- lookAhead (optional letterChar)
  case followedByLetter of
    Just _ -> fail $ "Invalid token: number followed by letter: " <> digits
    Nothing -> return $ TokNumber (read digits)

pSymbol :: Text -> CToken -> Parser CToken
pSymbol sym tok = lexeme $ try $ string sym $> tok

-- Parser for any single token
pToken :: Parser CToken
pToken = choice
  [ pKeyword "int"
  , pKeyword "void"
  , pKeyword "return"
  , pSymbol "(" TokLParen
  , pSymbol ")" TokRParen
  , pSymbol "{" TokLBrace
  , pSymbol "}" TokRBrace
  , pSymbol ";" TokSemicolon
  , pIdentifier
  , pNumber
  ]

-- Parse all tokens in a file
lexer :: Parser [CToken]
lexer = between scn eof (many pToken)