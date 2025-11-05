{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Halcyon.Frontend.Tokens
  ( -- * Token Types
    CToken (..),

    -- * Error Types
    CLexError (..),
    CParseError (..),
    SyntaxError (..),

    -- * Error Display
    formatLexError,
    formatParseError,
  )
where

import Data.List.NonEmpty qualified as NE
import Data.Proxy
import Data.Text
import Data.Text qualified as T
import Text.Megaparsec

-- | Tokens produced by lexical analysis
data CToken
  = -- Values
    Identifier Text
  | Number Int
  | -- Keywords
    KwInt
  | KwVoid
  | KwReturn
  | -- Punctuation
    LParen
  | RParen
  | LBrace
  | RBrace
  | Semicolon
  | Hyphen
  | DoubleHyphen
  | Tilde
  | Plus
  | Star
  | Slash
  | Percent
  deriving (Eq, Show, Ord)

-- | Common syntax errors that can occur in both lexing and parsing
data SyntaxError
  = -- | Character and context
    UnexpectedChar Char Text
  | -- | What was being parsed
    UnexpectedEnd Text
  | -- | What was found and what was expected
    InvalidSequence Text Text
  deriving (Eq, Ord, Show)

-- | Errors specific to lexical analysis
data CLexError
  = -- | Character not allowed in C
    InvalidCharacter Char
  | -- | Invalid numeric literal
    MalformedNumber Text
  | -- | String/char literal not closed
    UnterminatedLiteral Text
  | -- | Invalid escape sequence
    InvalidEscape Text
  | -- | Common syntax errors
    LexicalError SyntaxError
  deriving (Eq, Ord, Show)

-- | Errors specific to parsing
data CParseError
  = -- | Found unexpected token
    UnexpectedToken CToken Text
  | -- | Required token not found
    MissingToken CToken Text
  | -- | Invalid syntax construct
    InvalidConstruct Text
  | -- | Common syntax errors
    ParseError SyntaxError
  deriving (Eq, Ord, Show)

instance ShowErrorComponent CLexError where
  showErrorComponent = T.unpack . formatLexError

instance ShowErrorComponent CParseError where
  showErrorComponent = T.unpack . formatParseError

formatLexError :: CLexError -> Text
formatLexError = \case
  InvalidCharacter c ->
    "Invalid character in source: " <> T.singleton c
  MalformedNumber t ->
    "Malformed number literal: " <> t
  UnterminatedLiteral t ->
    "Unterminated literal: " <> t
  InvalidEscape t ->
    "Invalid escape sequence: " <> t
  LexicalError e -> formatSyntaxError e

formatParseError :: CParseError -> Text
formatParseError = \case
  UnexpectedToken tok ctx ->
    "Unexpected " <> T.pack (Prelude.show tok) <> " in " <> ctx
  MissingToken tok ctx ->
    "Missing " <> T.pack (Prelude.show tok) <> " in " <> ctx
  InvalidConstruct ctx ->
    "Invalid syntax in " <> ctx
  ParseError e -> formatSyntaxError e

formatSyntaxError :: SyntaxError -> Text
formatSyntaxError = \case
  UnexpectedChar c ctx ->
    "Unexpected '" <> T.singleton c <> "' in " <> ctx
  UnexpectedEnd ctx ->
    "Unexpected end of input while parsing " <> ctx
  InvalidSequence found expected ->
    "Invalid sequence: found " <> found <> ", expected " <> expected

-- | Provides human-readable descriptions of tokens for error messages
instance ShowErrorComponent CToken where
  showErrorComponent :: CToken -> String
  showErrorComponent = \case
    KwInt -> "keyword 'int'"
    KwVoid -> "keyword 'void'"
    KwReturn -> "keyword 'return'"
    Identifier t -> "identifier '" <> T.unpack t <> "'"
    Number n -> "number " <> Prelude.show n
    LParen -> "left parenthesis '('"
    RParen -> "right parenthesis ')'"
    LBrace -> "left brace '{'"
    RBrace -> "right brace '}'"
    Semicolon -> "semicolon ';'"
    Tilde -> "tilde '~'"
    Hyphen -> "hyphen '-'"
    DoubleHyphen -> "double hyphen '--'"
    Plus -> "plus '+'"
    Star -> "star '*'"
    Slash -> "slash '/'"
    Percent -> "percent '%'"

-- For better error messages - shows a preview of tokens
instance VisualStream [CToken] where
  showTokens :: Proxy [CToken] -> NE.NonEmpty (Token [CToken]) -> String
  showTokens Proxy = Prelude.show . NE.take 10

  tokensLength :: Proxy [CToken] -> NE.NonEmpty (Token [CToken]) -> Int
  tokensLength Proxy = NE.length

-- | Allows megaparsec to track position in token stream
instance TraversableStream [CToken] where
  reachOffset :: Int -> PosState [CToken] -> (Maybe String, PosState [CToken])
  reachOffset o PosState {..} =
    ( Just "",
      PosState
        { pstateInput = Prelude.drop (o - pstateOffset) pstateInput,
          pstateOffset = o,
          pstateSourcePos = pstateSourcePos,
          pstateTabWidth = pstateTabWidth,
          pstateLinePrefix = ""
        }
    )
