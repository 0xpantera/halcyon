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
import qualified Assembly as Asm


import Control.Exception (bracket, catch, IOException)
import System.FilePath (isExtensionOf, replaceExtension, takeDirectory, takeFileName, replaceExtension, (</>), dropExtension)
import System.IO (hPutStrLn, stderr, stdout)
import Options.Applicative
import System.Process.Typed
import System.Directory (getCurrentDirectory, removeFile, makeAbsolute)
import System.Environment (getArgs)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Text.Megaparsec 
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

preprocess :: AppOptions -> IO FilePath
preprocess AppOptions{..} = do
  absFile <- makeAbsolute file
  hPutStrLn stderr $ "absFile: " ++ absFile
  let 
    fileDir = takeDirectory absFile
    fileName = takeFileName absFile
    outputFileName = replaceExtension fileName ".i"
    output = fileDir </> outputFileName
    processConfig = proc "gcc" ["-E", "-P", absFile, "-o", output]
  hPutStrLn stderr $ "outputFileName: " ++ outputFileName
  hPutStrLn stderr $ "output: " ++ output
  (exitCode, _, _) <- readProcess processConfig
  if exitCode == ExitSuccess
    then return output
    else do
      hPutStrLn stderr "gcc failed to preprocess the file."
      exitFailure

runLexStage :: T.Text -> Either T.Text [CToken]
runLexStage input =
  case runParser lexer "placeholder" input of
    Left err -> Left $ T.pack (errorBundlePretty err)
    Right ctokens -> Right ctokens

runParseStage :: [CToken] -> Either T.Text Program
runParseStage ctokens =
  case Parse.parse ctokens of
    Left err -> Left $ T.pack (errorBundlePretty err)
    Right ast -> Right ast

runCodegenStage :: Ast.Program -> Either T.Text Asm.Program
runCodegenStage astProgram =
  case gen astProgram of
    Left err -> Left $ "Codegen failed: " <> err
    Right asm -> Right asm

runEmitStage :: Asm.Program -> Either T.Text T.Text
runEmitStage asmProgram = Right $ emitProgram asmProgram

processStage :: AppOptions -> T.Text -> Either T.Text StageResult
processStage AppOptions{..} src = case stage of
  Lex -> do
    ctokens <- runLexStage src
    pure $ StageResultTokens ctokens
  Parse -> do
    ctokens <- runLexStage src
    ast <- runParseStage ctokens
    pure $ StageResultAST ast
  Codegen -> do
    ctokens <- runLexStage src
    ast <- runParseStage ctokens
    asm <- runCodegenStage ast
    pure $ StageResultAsm asm
  Assembly -> do
    ctokens <- runLexStage src
    ast <- runParseStage ctokens
    asm <- runCodegenStage ast
    assembly <- runEmitStage asm
    pure $ StageResultAssembly assembly
  Executable -> do
    ctokens <- runLexStage src
    ast <- runParseStage ctokens
    asm <- runCodegenStage ast
    assembly <- runEmitStage asm
    pure $ StageResultExecutable assembly

handleStage :: AppOptions -> StageResult -> IO ()
handleStage AppOptions{file} stageRes = case stageRes of
  StageResultTokens ctokens -> do
    hPutStrLn stderr "Lexer succeeded."
    putStrLn $ "Tokens:\n" ++ show ctokens
  StageResultAST ast -> do
    hPutStrLn stderr "Parser succeeded."
    putStrLn $ "AST:\n" ++ show ast
  StageResultAsm asm -> do
    hPutStrLn stderr "Codegen succeeded."
    putStrLn $ "ASM:\n" ++ show asm
  StageResultAssembly assembly -> do
    hPutStrLn stderr "Assembly generation succeeded."
    putStrLn $ "Assembly Code:\n" ++ T.unpack assembly
  StageResultExecutable assembly -> do
    hPutStrLn stderr "Compiling to executable ..."
    inputFileAbs <- makeAbsolute file
    let
       -- Extract directory and base name from input file
      inputDir = takeDirectory inputFileAbs
      inputFileName = takeFileName inputFileAbs      -- e.g., "program.c"
      baseName = dropExtension inputFileName         -- e.g., "program"
      -- Define paths for assembly and executable files
      asmFileName = baseName ++ ".s"                 -- e.g., "program.s"
      asmFilePath = inputDir </> asmFileName         -- e.g., "/path/to/program.s"
      outputFileName = baseName                      -- e.g., "program"
      outputFilePath = inputDir </> outputFileName   -- e.g., "/path/to/program"
    -- Write the assembly file
    TIO.writeFile asmFilePath assembly
    hPutStrLn stderr $ "Assembly file: " ++ asmFilePath
    hPutStrLn stderr $ "Executable output: " ++ outputFilePath

    let processConfig = proc "gcc" [asmFilePath, "-o", outputFilePath]
    (exitCode, _, gccError) <- readProcess processConfig
    if exitCode == ExitSuccess
    then putStrLn "Succesfully compiled executable"
    else do
      hPutStrLn stderr ("gcc failed to compile the executable: " <> show gccError)
      --removeFile asmFilePath `catch` \_ -> return ()
      exitFailure

main :: IO ()
main = do
  args <- getArgs
  let debugMsg = "Args: " ++ show args ++ "\n"
  hPutStrLn stdout debugMsg
  
  opts <- execParser opt
  let optionsMsg = "Parsed options: " ++ show opts ++ "\n"
  hPutStrLn stderr optionsMsg

  preprocessedFile <- preprocess opts
  srcResult <- TIO.readFile preprocessedFile
  case processStage opts srcResult of
    Left err -> do
      hPutStrLn stderr $ "Error: " <> T.unpack err
      exitFailure
    Right result -> handleStage opts result
  where
    opt = info (helper <*> appOptionsParser) $
      progDesc "A compiler for a subset of C. Written in Haskell."
      <> header "A not-quite-C compiler"
