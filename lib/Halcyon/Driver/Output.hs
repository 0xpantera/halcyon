{-# LANGUAGE OverloadedStrings #-}
module Halcyon.Driver.Output
  ( handleStageResult
  ) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.IO as TIO

import Halcyon.Core.Monad (MonadCompiler)
import Halcyon.Core.Settings (StageResult(..))
import Halcyon.Driver.Cli (AppOptions(..))
import Halcyon.Driver.External (compileToExecutable)

-- | Handle the result of a compilation stage
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
