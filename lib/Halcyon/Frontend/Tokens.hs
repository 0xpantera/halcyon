{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
module Halcyon.Frontend.Tokens ( -- * Token Types
    CToken(..)
    -- * Error Types
  , CLexError(..)
  , CParseError(..)
  , SyntaxError(..)
    -- * Error Display
  , formatLexError
  , formatParseError
  ) where

import Data.Text
import qualified Data.Text as T
import Data.Proxy
import Text.Megaparsec
import qualified Data.List.NonEmpty as NE

-- | Tokens produced by lexical analysis
data CToken
  = TokInt       
  | TokVoid    
  | TokReturn 
  | TokIdent Text
  | TokNumber Int
  | TokLParen    
  | TokRParen    
  | TokLBrace    
  | TokRBrace    
  | TokSemicolon
  | TokHyphen
  | TokDoubleHyphen  
  | TokTilde
  deriving (Eq, Show, Ord)

-- | Common syntax errors that can occur in both lexing and parsing
data SyntaxError
  = UnexpectedChar Char Text  -- ^ Character and context
  | UnexpectedEnd Text        -- ^ What was being parsed
  | InvalidSequence Text Text -- ^ What was found and what was expected
  deriving (Eq, Ord, Show)

-- | Errors specific to lexical analysis
data CLexError
  = InvalidCharacter Char            -- ^ Character not allowed in C
  | MalformedNumber Text            -- ^ Invalid numeric literal
  | UnterminatedLiteral Text        -- ^ String/char literal not closed
  | InvalidEscape Text              -- ^ Invalid escape sequence
  | LexicalError SyntaxError        -- ^ Common syntax errors
  deriving (Eq, Ord, Show)

-- | Errors specific to parsing
data CParseError
  = UnexpectedToken CToken Text     -- ^ Found unexpected token
  | MissingToken CToken Text        -- ^ Required token not found 
  | InvalidConstruct Text           -- ^ Invalid syntax construct
  | ParseError SyntaxError          -- ^ Common syntax errors
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
    "Unexpected " <> T.pack (show tok) <> " in " <> ctx
  MissingToken tok ctx ->
    "Missing " <> T.pack (show tok) <> " in " <> ctx
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
    TokInt -> "keyword 'int'"
    TokVoid -> "keyword 'void'"
    TokReturn -> "keyword 'return'"
    TokIdent t -> "identifier '" <> T.unpack t <> "'"
    TokNumber n -> "number " <> show n
    TokLParen -> "left parenthesis '('"
    TokRParen -> "right parenthesis ')'"
    TokLBrace -> "left brace '{'"
    TokRBrace -> "right brace '}'"
    TokSemicolon -> "semicolon ';'"
    TokTilde -> "tilde '~'"
    TokHyphen -> "hyphen '-'"
    TokDoubleHyphen -> "double hyphen '--'"

-- For better error messages - shows a preview of tokens
instance VisualStream [CToken] where
  showTokens :: Proxy [CToken] -> NE.NonEmpty (Token [CToken]) -> String
  showTokens Proxy = show . NE.take 10

  tokensLength :: Proxy [CToken] -> NE.NonEmpty (Token [CToken]) -> Int
  tokensLength Proxy = NE.length

-- | Allows megaparsec to track position in token stream
instance TraversableStream [CToken] where
  reachOffset :: Int -> PosState [CToken] -> (Maybe String, PosState [CToken])
  reachOffset o PosState{..} =
      ( Just ""
      , PosState
          { pstateInput = Prelude.drop (o - pstateOffset) pstateInput
          , pstateOffset = o
          , pstateSourcePos = pstateSourcePos
          , pstateTabWidth = pstateTabWidth
          , pstateLinePrefix = ""
          }
      )
