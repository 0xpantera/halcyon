{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Halcyon.Driver.Pipeline
  ( StageResult(..)
  , processFile
  ) where


import Control.Monad.IO.Class (liftIO)
import System.Exit (exitFailure)
import Data.Text (Text)
import qualified Data.Text.IO as TIO

import Halcyon.Core (MonadCompiler, runCompiler, formatError)
import Halcyon.Core.Settings (Stage(..), StageResult(..))
import Halcyon.Driver.Cli (AppOptions(..))
import Halcyon.Driver.External (preprocess)
import Halcyon.Driver.Output (handleStageResult) 
import Halcyon.Driver.Stages
  ( runLexStage
  , runParseStage
  , runTackyStage
  , runCodegenStage
  , runFixupStage
  , runEmitStage
  , debugStage
  )


-- | Run compilation stages based on command line options
runCompilerStages :: MonadCompiler m => AppOptions -> Text -> m StageResult
runCompilerStages AppOptions{..} src = case stage of
  Lex -> StageResultTokens <$> runLexStage src
  Parse -> StageResultAST <$> 
    (runLexStage src 
    >>= runParseStage)
  Tacky -> StageResultTacky <$>
    (runLexStage src 
    >>= runParseStage 
    >>= debugStage "AST" 
    >>= runTackyStage)
  Codegen -> StageResultAsm <$> 
    (runLexStage src 
    >>= runParseStage 
    >>= runTackyStage
    >>= debugStage "TACKY" 
    >>= runCodegenStage)
  Assembly -> StageResultAssembly <$>
    (runLexStage src 
    >>= runParseStage 
    >>= runTackyStage 
    >>= runCodegenStage 
    >>= runFixupStage 
    >>= debugStage "ASM" 
    >>= runEmitStage)
  Executable -> StageResultExecutable <$>
    (runLexStage src 
    >>= runParseStage 
    >>= runTackyStage 
    >>= runCodegenStage 
    >>= runFixupStage 
    >>= runEmitStage)

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