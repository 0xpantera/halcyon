{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Halcyon.Driver.Pipeline
  ( StageResult(..)
  , processFile
  , handleStageResult
  ) where

import Halcyon.Driver.Cli (AppOptions(..), Stage(..))
import qualified Halcyon.Frontend.Lexer as Lexer
import qualified Halcyon.Frontend.Parse as Parse
import qualified Halcyon.Backend.Codegen as Codegen
import qualified Halcyon.Backend.Emit as Emit
import qualified Halcyon.Core.Assembly as Asm
import qualified Halcyon.Core.Ast as Ast
import Halcyon.Core.Settings ( StageResult(..) )
import qualified Halcyon.Frontend.Tokens as Tokens

import Control.Monad (when)
import System.Exit (exitFailure)
import System.FilePath
import System.Directory (makeAbsolute)
import System.Process.Typed
import System.IO (hPutStrLn, stderr)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Text.Megaparsec (errorBundlePretty, runParser)


preprocess :: AppOptions -> IO FilePath
preprocess AppOptions{..} = do
  absFile <- makeAbsolute file
  let 
    fileDir = takeDirectory absFile
    fileName = takeFileName absFile
    outputFileName = replaceExtension fileName ".i"
    output = fileDir </> outputFileName
    processConfig = proc "gcc" ["-E", "-P", absFile, "-o", output]
  
  (exitCode, _, _) <- readProcess processConfig
  if exitCode == ExitSuccess
    then return output
    else do
      hPutStrLn stderr "gcc failed to preprocess the file."
      exitFailure

runCompilerStages :: AppOptions -> T.Text -> Either T.Text StageResult
runCompilerStages AppOptions{..} src = case stage of
  Lex -> StageResultTokens <$> runLexStage src
  Parse -> StageResultAST <$> 
    (runLexStage src >>= runParseStage)
  Codegen -> StageResultAsm <$> 
    (runLexStage src >>= runParseStage >>= runCodegenStage)
  Assembly -> StageResultAssembly <$>
    (runLexStage src >>= runParseStage >>= runCodegenStage >>= runEmitStage)
  Executable -> StageResultExecutable <$>
    (runLexStage src >>= runParseStage >>= runCodegenStage >>= runEmitStage)
  where
    runLexStage :: T.Text -> Either T.Text [Tokens.CToken]
    runLexStage input = case runParser Lexer.lexer "" input of
      Left err -> Left $ T.pack $ errorBundlePretty err
      Right tokens -> Right tokens
    
    runParseStage :: [Tokens.CToken] -> Either T.Text Ast.Program
    runParseStage tokens = case Parse.parseTokens tokens of
      Left err -> Left $ T.pack $ errorBundlePretty err
      Right ast -> Right ast
    
    runCodegenStage :: Ast.Program -> Either T.Text Asm.Program
    runCodegenStage = Codegen.gen
    
    runEmitStage :: Asm.Program -> Either T.Text T.Text
    runEmitStage = Right . Emit.emitProgram

handleStageResult :: AppOptions -> StageResult -> IO ()
handleStageResult opts@AppOptions{} result = case result of
  StageResultTokens tokens -> do
    hPutStrLn stderr "Lexer succeeded."
    print tokens
    
  StageResultAST ast -> do
    hPutStrLn stderr "Parser succeeded."
    print ast
    
  StageResultAsm asm -> do
    hPutStrLn stderr "Codegen succeeded."
    print asm
    
  StageResultAssembly assembly -> do
    hPutStrLn stderr "Assembly generation succeeded."
    putStrLn $ T.unpack assembly
    
  StageResultExecutable assembly -> 
    compileToExecutable opts assembly

compileToExecutable :: AppOptions -> T.Text -> IO ()
compileToExecutable AppOptions{..} assembly = do
  inputFileAbs <- makeAbsolute file
  let
    inputDir = takeDirectory inputFileAbs
    baseName = dropExtension $ takeFileName inputFileAbs
    asmFilePath = inputDir </> (baseName ++ ".s")
    outputFilePath = inputDir </> baseName

  TIO.writeFile asmFilePath assembly
  
  let processConfig = proc "gcc" [asmFilePath, "-o", outputFilePath]
  (exitCode, _, gccError) <- readProcess processConfig
  if exitCode == ExitSuccess
    then putStrLn "Successfully compiled executable"
    else do
      hPutStrLn stderr $ "gcc failed to compile: " ++ show gccError
      exitFailure

processFile :: AppOptions -> IO ()
processFile opts = do
  preprocessedFile <- preprocess opts
  srcResult <- TIO.readFile preprocessedFile
  case runCompilerStages opts srcResult of
    Left err -> do
      hPutStrLn stderr $ "Error: " <> T.unpack err
      exitFailure
    Right result -> handleStageResult opts result