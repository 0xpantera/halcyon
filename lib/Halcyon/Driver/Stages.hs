{-# LANGUAGE OverloadedStrings #-}
module Halcyon.Driver.Stages
  ( -- * Individual Stage Runners
    runLexStage
  , runParseStage  
  , runTackyStage
  , runCodegenStage
  , runFixupStage
  , runEmitStage
  , debugStage
  ) where

import Control.Monad.IO.Class
import Data.Text ( Text )
import Data.Text qualified as T
import Text.Megaparsec (runParser)
import qualified System.IO as SIO

import Halcyon.Core 
  ( MonadCompiler
  , liftLexResult
  , liftParseResult
  , throwError
  , CompilerError(..)
  , genTacky )
import Halcyon.Core.Ast qualified as Ast
import Halcyon.Core.Assembly qualified as Asm
import Halcyon.Core.Tacky qualified as Tacky
import Halcyon.Frontend (lexer, parseTokens, CToken)
import Halcyon.Backend (gen, emitProgram, replacePseudos, fixupProgram)

-- | Run the lexer on input text
runLexStage :: MonadCompiler m => Text -> m [CToken]
runLexStage input = liftLexResult $ runParser lexer "" input

-- | Parse tokens into AST
runParseStage :: MonadCompiler m => [CToken] -> m Ast.Program
runParseStage = liftParseResult . parseTokens

-- | Generate TACKY IR from AST
runTackyStage :: MonadCompiler m => Ast.Program -> m Tacky.Program
runTackyStage = genTacky

-- | Generate assembly from TACKY
runCodegenStage :: MonadCompiler m => Tacky.Program -> m Asm.Program
runCodegenStage = return . gen

-- | Replace pseudo registers with stack locations
runFixupStage :: MonadCompiler m => Asm.Program -> m Asm.Program
runFixupStage program = do
  case replacePseudos program of
    Left err -> 
      throwError $ CodegenError $ "Pseudo replacement failed: " <> T.pack (show err)
    Right (programWithStacks, lastOffset) -> 
      return $ fixupProgram programWithStacks lastOffset

-- | Convert assembly to text
runEmitStage :: MonadCompiler m => Asm.Program -> m Text
runEmitStage = return . emitProgram

debugStage :: (Show a, MonadCompiler m) => String -> a -> m a 
debugStage label x = do
  liftIO $ SIO.hPutStrLn SIO.stderr $ "\n=== " ++ label ++ " ===\n" ++ show x
  liftIO $ SIO.hFlush SIO.stderr
  return x
