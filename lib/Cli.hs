{-# LANGUAGE OverloadedStrings #-}
module Cli
  ( AppOptions(..)
  , Stage(..)
  , parseOptions
  ) where

import Options.Applicative
import System.FilePath (isExtensionOf)
import Settings ( Stage(..) )

data AppOptions = AppOptions 
  { file  :: FilePath
  , stage :: Stage
  } deriving (Eq, Show)

stageParser :: Parser Stage
stageParser = 
      flag' Lex     (long "lex"     <> help "Run the lexer")
  <|> flag' Parse   (long "parse"   <> help "Run the lexer and parser")
  <|> flag' Codegen (long "codegen" <> help "Run through code generation")
  <|> flag' Assembly (long "S"      <> help "Stop before assembling")
  <|> pure Executable  -- Default value if no flag is provided

inputFileParser :: Parser FilePath
inputFileParser =
  argument validFile 
    ( metavar "FILE"
      <> help "Input C file" )

validExtension :: FilePath -> Bool
validExtension path = 
  ".c" `isExtensionOf` path || ".h" `isExtensionOf` path

validFile :: ReadM FilePath
validFile = eitherReader $ \arg ->
  if validExtension arg
  then return arg
  else Left "Invalid file extension. Expected a .c or .h file."

appOptionsParser :: Parser AppOptions
appOptionsParser = AppOptions
  <$> inputFileParser
  <*> stageParser

parseOptions :: IO AppOptions
parseOptions = execParser opts
  where
    opts = info (helper <*> appOptionsParser) $
      progDesc "A compiler for a subset of C. Written in Haskell."
      <> header "A not-quite-C compiler"