{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Halcyon.Driver.External
  ( -- * GCC Interface
    preprocess
  , compileToExecutable
  ) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Exit (ExitCode(..))
import System.FilePath (replaceExtension, takeDirectory, takeFileName, dropExtension, (</>))
import System.Directory (makeAbsolute)
import System.Process.Typed (proc, readProcess)

import Halcyon.Core.Monad (MonadCompiler, CompilerError(..), throwError)
import Halcyon.Driver.Cli (AppOptions(..))

-- | Preprocess source file using GCC
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

-- | Compile assembly to executable using GCC
compileToExecutable :: MonadCompiler m => AppOptions -> Text -> m ()
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