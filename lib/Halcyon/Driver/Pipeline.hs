{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Halcyon.Driver.Pipeline
  ( StageResult(..)
  , processFile
  , handleStageResult
  ) where

import Halcyon.Core.Monad
import qualified Halcyon.Core.Tacky as Tacky
import Halcyon.Core.TackyGen
import Halcyon.Driver.Cli (AppOptions(..), Stage(..))
import qualified Halcyon.Frontend.Lexer as Lexer
import qualified Halcyon.Frontend.Parse as Parse
import qualified Halcyon.Backend.Codegen as Codegen
import qualified Halcyon.Backend.Emit as Emit
import qualified Halcyon.Core.Assembly as Asm
import qualified Halcyon.Core.Ast as Ast
import Halcyon.Core.Settings (StageResult(..))
import qualified Halcyon.Frontend.Tokens as Tokens

import Control.Monad (unless)
import Control.Monad.Except ()
import Control.Monad.IO.Class ()
import System.Exit (exitFailure)
import System.FilePath
import System.Directory (makeAbsolute)
import System.Process.Typed
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Text.Megaparsec (runParser)


-- | Preprocess the source file using GCC
preprocess :: MonadCompiler m => AppOptions -> m FilePath
preprocess AppOptions{..} = do
  absFile <- liftIO $ makeAbsolute file
  let fileDir = takeDirectory absFile
      fileName = takeFileName absFile
      outputFileName = replaceExtension fileName ".i"
      output = fileDir </> outputFileName
      processConfig = proc "gcc" ["-E", "-P", absFile, "-o", output]
  
  (exitCode, _, stderr) <- liftIO $ readProcess processConfig
  if exitCode == ExitSuccess
    then return output
    else throwError $ SystemError $ "GCC preprocessing failed: " <> T.pack (show stderr)

-- | Run compilation stages based on command line options
runCompilerStages :: MonadCompiler m => AppOptions -> T.Text -> m StageResult
runCompilerStages AppOptions{..} src = case stage of
  Lex -> StageResultTokens <$> runLexStage src
  Parse -> StageResultAST <$> 
    (runLexStage src >>= runParseStage)
  Tacky -> StageResultTacky <$>
    (runLexStage src >>= runParseStage >>= runTackyStage)
  Codegen -> StageResultAsm <$> 
    (runLexStage src >>= runParseStage >>= runCodegenStage)
  Assembly -> StageResultAssembly <$>
    (runLexStage src >>= runParseStage >>= runCodegenStage >>= runEmitStage)
  Executable -> StageResultExecutable <$>
    (runLexStage src >>= runParseStage >>= runCodegenStage >>= runEmitStage)
  where
    runLexStage :: MonadCompiler m => T.Text -> m [Tokens.CToken]
    runLexStage input = liftLexResult $ runParser Lexer.lexer "" input
    runParseStage :: MonadCompiler m => [Tokens.CToken] -> m Ast.Program
    runParseStage = liftParseResult . Parse.parseTokens
    runTackyStage :: MonadCompiler m => Ast.Program -> m Tacky.Program
    runTackyStage = genTacky
    runCodegenStage :: MonadCompiler m => Ast.Program -> m Asm.Program
    runCodegenStage = liftCompilerEither . Codegen.gen
    runEmitStage :: MonadCompiler m => Asm.Program -> m T.Text
    runEmitStage = return . Emit.emitProgram

-- | Handle the result of compilation
handleStageResult :: MonadCompiler m => AppOptions -> StageResult -> m ()
handleStageResult opts@AppOptions{} = \case
  StageResultTokens tokens -> 
    liftIO $ print tokens
  StageResultAST ast ->
    liftIO $ print ast
  StageResultTacky tacky ->
    liftIO $ print tacky
  StageResultAsm asm ->
    liftIO $ print asm
  StageResultAssembly assembly ->
    liftIO $ TIO.putStrLn assembly
  StageResultExecutable assembly -> 
    compileToExecutable opts assembly

-- | Compile assembly to an executable using GCC
compileToExecutable :: MonadCompiler m => AppOptions -> T.Text -> m ()
compileToExecutable AppOptions{..} assembly = do
  inputFileAbs <- liftIO $ makeAbsolute file
  let inputDir = takeDirectory inputFileAbs
      baseName = dropExtension $ takeFileName inputFileAbs
      asmFilePath = inputDir </> (baseName ++ ".s")
      outputFilePath = inputDir </> baseName

  liftIO $ TIO.writeFile asmFilePath assembly
  
  let processConfig = proc "gcc" [asmFilePath, "-o", outputFilePath]
  (exitCode, _, stderr) <- liftIO $ readProcess processConfig
  
  unless (exitCode == ExitSuccess) $
    throwError $ SystemError $ "GCC compilation failed: " <> T.pack (show stderr)

-- | Main entry point for the compiler pipeline
processFile :: AppOptions -> IO ()
processFile opts = do
  result <- runCompiler $ do
    preprocessedFile <- preprocess opts
    srcResult <- liftIO $ TIO.readFile preprocessedFile
    result <- runCompilerStages opts srcResult
    handleStageResult opts result
    
  case result of
    Left err -> do
      TIO.putStrLn $ "Error: " <> formatError err
      exitFailure
    Right _ -> 
      pure ()