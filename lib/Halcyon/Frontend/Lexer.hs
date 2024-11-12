{-# LANGUAGE OverloadedStrings #-}
module Halcyon.Frontend.Lexer where

import Data.Functor (($>))
import Data.Text (Text)
import Data.Void ( Void )
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Text as T

import Halcyon.Frontend.Tokens

-- Parser type
type Lexer = Parsec CLexError Text

-- Space consumer that handles newlines (used between tokens)
scn :: Lexer ()
scn = L.space
  space1  -- handles spaces, tabs, and newlines
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

lexeme :: Lexer a -> Lexer a
lexeme = L.lexeme scn

symbol :: Text -> Lexer Text
symbol = L.symbol scn

-- Individual token parsers
pKeyword :: Text -> Lexer CToken
pKeyword kw = lexeme $ try $ do
  _ <- string kw
  notFollowedBy alphaNumChar
  return $ case kw of
    "int"    -> TokInt
    "void"   -> TokVoid
    "return" -> TokReturn
    _        -> error "Invalid keyword"

pIdentifier :: Lexer CToken
pIdentifier = lexeme $ try $ do
  first <- letterChar <|> char '_'
  rest <- many (alphaNumChar <|> char '_')
  let ident = T.pack (first:rest)
  return $ TokIdent ident

pNumber :: Lexer CToken
pNumber = lexeme $ try $ do
  digits <- some digitChar
  followedByLetter <- lookAhead (optional letterChar)
  case followedByLetter of
    Just _ -> fail $ "Invalid token: number followed by letter: " <> digits
    Nothing -> return $ TokNumber (read digits)

pSymbol :: Text -> CToken -> Lexer CToken
pSymbol sym tok = lexeme $ try $ string sym $> tok

-- Parser for any single token
pToken :: Lexer CToken
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
lexer :: Lexer [CToken]
lexer = between scn eof (many pToken)