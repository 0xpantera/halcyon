{-# LANGUAGE OverloadedStrings #-}
module Halcyon.Frontend.Lexer where

import Data.Functor (($>))
import Data.Text (Text)
import Data.Set (Set)
import qualified Data.Set as Set
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
  space1  
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
  notFollowedBy alphaNumChar <?> 
    "end of keyword '" <> T.unpack kw <> "'"
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
  -- Check for valid identifier
  if T.length ident > 31
    then customFailure $ LexicalError $ InvalidSequence ident 
      "identifier must be 31 characters or less"
    else return $ TokIdent ident

pNumber :: Lexer CToken
pNumber = lexeme $ try $ do
  digits <- some digitChar
  followedByLetter <- lookAhead (optional alphaNumChar)
  case followedByLetter of
    Just c -> customFailure $ MalformedNumber $ 
      "Invalid character '" <> T.singleton c <> "' in number"
    Nothing -> 
      let n = read digits
      in if n > (2^31 - 1) 
         then customFailure $ MalformedNumber "Integer too large"
         else return $ TokNumber n

pSymbol :: Text -> CToken -> Lexer CToken
pSymbol sym tok = lexeme $ try $ 
  ((string sym $> tok) <?> T.unpack sym)

-- Parser for any single token
pToken :: Lexer CToken
pToken = choice
  [ pKeyword "int"
  , pKeyword "void"
  , pKeyword "return"
  , pSymbol "("  TokLParen
  , pSymbol ")"  TokRParen
  , pSymbol "{"  TokLBrace
  , pSymbol "}"  TokRBrace
  , pSymbol ";"  TokSemicolon
  , pSymbol "-"  TokHyphen
  , pSymbol "--" TokDoubleHyphen
  , pSymbol "~"  TokTilde
  , pIdentifier
  , pNumber
  ] <?> "token"

-- Parse all tokens in a file
lexer :: Lexer [CToken]
lexer = between scn eof (many pToken) <?> "C program"