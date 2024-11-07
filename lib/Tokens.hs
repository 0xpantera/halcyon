{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
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

-- Lets megaparsec show nice error messages for our token type
instance ShowErrorComponent CToken where
  showErrorComponent = show

-- For better error messages
instance VisualStream [CToken] where
  showTokens Proxy = show . NE.take 10
  tokensLength Proxy = length

instance TraversableStream [CToken] where
  reachOffset o PosState{..} =
    ( Just ""  -- For token streams, we don't need to show any prefix
    , PosState
        { pstateInput = drop (o - pstateOffset) pstateInput
        , pstateOffset = o
        , pstateSourcePos = pstateSourcePos
        , pstateTabWidth = pstateTabWidth
        , pstateLinePrefix = ""
        }
    )
