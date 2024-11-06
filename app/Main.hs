{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Settings

import System.FilePath (isExtensionOf, replaceExtension)
import Options.Applicative
import System.Process.Typed
import System.Directory (getCurrentDirectory)

data AppOptions = AppOptions 
  { file  :: FilePath
  , stage :: Stage
  } deriving (Eq, Show)

stageParser :: Parser Stage
stageParser =
  flag' Lex 
    ( long "lex"
      <> help "Run the lexer" )
  <|> flag' Parse
    ( long "parse"
      <> help "Run the lexer and parser" )
  <|> flag' Codegen
    ( long "codegen"
      <> help "Run through code generation but stop before emitting assembly" )

inputFileParser :: Parser FilePath
inputFileParser =
  argument validFile 
    ( metavar "FILE"
      <> help "Input C file" )

appOptionsParser :: Parser AppOptions
appOptionsParser = AppOptions
  <$> inputFileParser
  <*> stageParser

validExtension :: FilePath -> Bool
validExtension path = 
  ".c" `isExtensionOf` path || ".h" `isExtensionOf` path

validFile :: ReadM FilePath
validFile = eitherReader $ \arg ->
  if validExtension arg
  then return arg
  else Left "Invalid file extension. Expected a .c or .h file."

replaceExt :: AppOptions -> FilePath
replaceExt AppOptions{..} = replaceExtension file ".i"

-- "gcc" [ "-E"; "-P"; src; "-o"; output ]
preprocess :: AppOptions -> IO (ProcessConfig () () ())
preprocess app@AppOptions{..} = do
  output <- pure $ replaceExt app
  cwd <- getCurrentDirectory
  pure $ setWorkingDir cwd $ proc "gcc" [ "-E", "-P", file, "-o", output ]


main :: IO ()
main = do
  options <- execParser opt
  processConfig <- preprocess options
  runProcess_ processConfig
  where
    opt = info (helper <*> appOptionsParser) $
      progDesc "A compiler for a subset of C. Written in Haskell."
      <> header "A not-quite-C compiler"
