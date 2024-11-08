{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Settings
import Lexer (lexer)
import Tokens
import Parse (parse)
import Ast
import Codegen
import Emit

import Control.Exception (bracket, catch, IOException)
import System.FilePath (isExtensionOf, replaceExtension, takeDirectory, takeFileName, replaceExtension, (</>))
import System.IO (hPutStrLn, stderr, stdout, appendFile)
import Options.Applicative
import System.Process.Typed
import System.Directory (getCurrentDirectory, removeFile, makeAbsolute)
import System.Environment (getArgs)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Text.Megaparsec (runParser, errorBundlePretty)
import Control.Monad (when, void)
import System.Exit (exitFailure)

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


-- Handle source file as positional argument
sourceFileParser :: Parser FilePath
sourceFileParser = argument str
  (  metavar "FILE"
  <> help "Input C file" )

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
preprocess' :: AppOptions -> IO (ProcessConfig () () ())
preprocess' app@AppOptions{..} = do
  output <- pure $ replaceExt app
  cwd <- getCurrentDirectory
  pure $ setWorkingDir cwd $ proc "gcc" [ "-E", "-P", file, "-o", output ]

preprocess :: AppOptions -> IO FilePath
preprocess app@AppOptions{..} = do
  -- Convert the source file path to an absolute path
  absFile <- makeAbsolute file
  hPutStrLn stderr $ "absFile: " ++ absFile
  -- Get the directory of the source file
  let fileDir = takeDirectory absFile
      -- Get just the file name
      fileName = takeFileName absFile
      -- Create the output file name with the .i extension
      outputFileName = replaceExtension fileName ".i"
      -- Construct the full output file path in the source file's directory
      output = fileDir </> outputFileName
  -- Prepare the process configuration without changing the working directory
  let processConfig = proc "gcc" ["-E", "-P", absFile, "-o", output]
  hPutStrLn stderr $ "outputFileName: " ++ outputFileName
  hPutStrLn stderr $ "output: " ++ output
  (exitCode, _, _) <- readProcess processConfig
  -- Check if gcc succeeded
  if exitCode == ExitSuccess
    then return output
    else do
      -- Print gcc's error messages
      --hPutStrLn stderr stderrBS
      -- Handle the error as appropriate
      hPutStrLn stderr "gcc failed to preprocess the file."
      exitFailure

-- | Run the lexer on the preprocessed file
runLexer :: FilePath -> IO [CToken]
runLexer preprocessedFile = do
  input <- TIO.readFile preprocessedFile
  case runParser lexer preprocessedFile input of
    Left err -> do
      putStrLn $ errorBundlePretty err
      exitFailure
    Right tokens -> return tokens

runLexStage :: FilePath -> IO [CToken]
runLexStage preprocessedFile = do
  input <- TIO.readFile preprocessedFile
  case runParser lexer preprocessedFile input of
    Left err -> do
      putStrLn $ errorBundlePretty err
      exitFailure
    Right tokens -> return tokens

processStage :: AppOptions -> FilePath -> IO ()
processStage AppOptions{..} preprocessedFile = do
  hPutStrLn stderr $ "Processing stage: " ++ show stage
  hPutStrLn stderr $ "Preprocessed file: " ++ preprocessedFile
  input <- TIO.readFile preprocessedFile  -- Read the file once

  bracket 
    (return ())  -- No special setup needed
    (\_ -> removeFile preprocessedFile `catch` \(e :: IOException) -> do
            hPutStrLn stderr $ "Warning: Could not remove temporary file: " ++ show e)
    (\_ -> case stage of
      Lex -> do
        hPutStrLn stderr "Running lexer..."
        input <- TIO.readFile preprocessedFile  
        hPutStrLn stderr $ "Read input: " ++ show input
        case runParser lexer preprocessedFile input of
          Left err -> do
            hPutStrLn stderr $ "Lexer error: " ++ errorBundlePretty err
            exitFailure
          Right tokens -> do
            hPutStrLn stderr $ "Lexer succeeded, tokens: " ++ show tokens
      Parse -> do
        hPutStrLn stderr "Running parser..."
        case runParser lexer preprocessedFile input of
          Left err -> do
            hPutStrLn stderr $ "Lexer error: " ++ errorBundlePretty err
            exitFailure
          Right tokens -> 
            case parse tokens of
              Left err -> do
                hPutStrLn stderr $ "Parser error: " ++ errorBundlePretty err
                exitFailure
              Right ast -> do
                hPutStrLn stderr $ "Parser succeeded, AST: " ++ show ast      
      Codegen -> do
        hPutStrLn stderr "Running codegen..."
        input <- TIO.readFile preprocessedFile
        case runParser lexer preprocessedFile input of
          Left err -> do
            hPutStrLn stderr $ "Lexer error: " ++ errorBundlePretty err
            exitFailure
          Right tokens -> 
            case parse tokens of
              Left err -> do
                hPutStrLn stderr $ "Parser error: " ++ errorBundlePretty err
                exitFailure
              Right ast ->
                case gen ast of
                  Left err -> do
                    hPutStrLn stderr $ "Codegen error: " ++ T.unpack err
                    exitFailure
                  Right asmAst -> do
                    hPutStrLn stderr $ "Codegen succeeded: " ++ show asmAst
      Assembly -> do
        hPutStrLn stderr "Running Assembly generation..."
        input <- TIO.readFile preprocessedFile
        case runParser lexer preprocessedFile input of
          Left err -> do
            hPutStrLn stderr $ "Lexer error: " ++ errorBundlePretty err
            exitFailure
          Right tokens -> 
            case parse tokens of
              Left err -> do
                hPutStrLn stderr $ "Parser error: " ++ errorBundlePretty err
                exitFailure
              Right ast ->
                case gen ast of
                  Left err -> do
                    hPutStrLn stderr $ "Codegen error: " ++ T.unpack err
                    exitFailure
                  Right asmAst -> do
                    hPutStrLn stderr $ "Codegen succeeded: " ++ show asmAst
                    -- TODO: ADD WRITING ASSEMBLY TO FILE
      Executable -> do
        tokens <- runLexStage preprocessedFile
        -- TODO: Complete steps
        return ()
    )
    --return ()


main :: IO ()
main = do
  args <- getArgs
  let debugMsg = "Args: " ++ show args ++ "\n"
  hPutStrLn stdout debugMsg
  
  opts <- execParser opt
  let optionsMsg = "Parsed options: " ++ show opts ++ "\n"
  hPutStrLn stderr optionsMsg

  preprocessedFile <- preprocess opts
  processStage opts preprocessedFile
  where
    opt = info (helper <*> appOptionsParser) $
      progDesc "A compiler for a subset of C. Written in Haskell."
      <> header "A not-quite-C compiler"
