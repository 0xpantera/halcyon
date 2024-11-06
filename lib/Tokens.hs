module Tokens where


data Token
  -- keywords
  = TokInt        -- int keyword
  | TokVoid       -- void keyword 
  | TokReturn     -- return keyword
  -- tokens with content
  | TokIdent Text -- identifiers
  | TokNumber Int -- numeric constants
  -- punctuation
  | TokLParen     -- (
  | TokRParen     -- )
  | TokLBrace     -- {
  | TokRBrace     -- }
  | TokSemicolon  -- ;
  deriving (Eq, Show)