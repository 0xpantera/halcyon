{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
module Tokens where

import qualified Data.Text as T
import Data.Proxy
import Text.Megaparsec
import qualified Data.List.NonEmpty as NE

data CToken
  = TokInt        
  | TokVoid      
  | TokReturn    
  | TokIdent T.Text
  | TokNumber Int
  | TokLParen    
  | TokRParen    
  | TokLBrace    
  | TokRBrace    
  | TokSemicolon 
  deriving (Eq, Show, Ord)

data CTokenParseError
  = UnexpectedToken CToken T.Text
  | CustomError T.Text
  deriving (Eq, Ord, Show)

instance ShowErrorComponent CTokenParseError where
  showErrorComponent (UnexpectedToken ctoken msg) =
    T.unpack msg <> ": " <> show ctoken
  showErrorComponent (CustomError msg) = T.unpack msg

-- Lets megaparsec show nice error messages for our token type
instance ShowErrorComponent CToken where
  showErrorComponent = show
  -- Should be better but try out normal one first
  -- showErrorComponent token = case token of
  --  TokInt       -> "keyword 'int'"
  --  TokVoid      -> "keyword 'void'"
  --  TokReturn    -> "keyword 'return'"
  --  TokIdent t   -> "identifier '" ++ T.unpack t ++ "'"
  --  TokNumber n  -> "number " ++ show n
  --  TokLParen    -> "left parenthesis '('"
  --  TokRParen    -> "right parenthesis ')'"
  --  TokLBrace    -> "left brace '{'"
  --  TokRBrace    -> "right brace '}'"
  --  TokSemicolon -> "semicolon ';'"

-- For better error messages
instance VisualStream [CToken] where
  showTokens :: Proxy [CToken] -> NE.NonEmpty (Token [CToken]) -> String
  showTokens Proxy = show . NE.take 10

  tokensLength :: Proxy [CToken] -> NE.NonEmpty (Token [CToken]) -> Int
  tokensLength Proxy = length

instance TraversableStream [CToken] where
  reachOffset :: Int -> PosState [CToken] -> (Maybe String, PosState [CToken])
  reachOffset o PosState{..} =
      ( Just ""
      , PosState
          { pstateInput = drop (o - pstateOffset) pstateInput
          , pstateOffset = o
          , pstateSourcePos = pstateSourcePos
          , pstateTabWidth = pstateTabWidth
          , pstateLinePrefix = ""
          }
      )
