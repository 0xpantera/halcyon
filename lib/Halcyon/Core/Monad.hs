{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Halcyon.Core.Monad 
  ( -- * The Compiler Monad
    CompilerT
  , Compiler
  , CompilerError(..)
  , MonadCompiler
    -- * Running the Compiler
  , runCompilerT
  , runCompiler
    -- * Error Handling
  , throwError
  , catchError
    -- * Lifting Operations
  , liftCompilerEither
  , liftIO
  , noteCompilerError
    -- * Parser Error Handling
  , liftLexResult
  , liftParseResult
    -- * Error Formatting
  , formatError
  , toTestError
  ) where

import Control.Monad.Except
import Control.Monad.IO.Class
import qualified Text.Megaparsec as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Halcyon.Frontend.Tokens as Tokens

-- | Represents different types of errors that can occur during compilation.
-- This type wraps more specific error types from different compilation stages
-- into a single type that can be handled uniformly throughout the compiler.
data CompilerError
  = LexerError (M.ParseErrorBundle Text Tokens.CLexError)
    -- ^ Wraps lexer-specific errors that occur during tokenization
  | ParserError (M.ParseErrorBundle [Tokens.CToken] Tokens.CParseError) 
    -- ^ Wraps parser-specific errors that occur during AST construction
  | CodegenError Text
    -- ^ Errors during code generation
  | IOError Text
    -- ^ File system and IO related errors
  | SystemError Text
    -- ^ External tool errors (e.g., GCC)
  deriving (Show, Eq)

-- | The compiler monad transformer stack. This provides:
--   * Error handling through ExceptT
--   * IO capabilities through the underlying monad
newtype CompilerT m a = CompilerT 
  { unCompilerT :: ExceptT CompilerError m a }
  deriving 
    ( Functor
    , Applicative 
    , Monad
    , MonadError CompilerError
    , MonadIO
    )

-- | Concrete compiler monad for direct execution
type Compiler = CompilerT IO

-- | Constraint for monads that can perform compiler operations
type MonadCompiler m = 
  ( MonadError CompilerError m
  , MonadIO m
  )

-- | Run a compiler computation in any monad
runCompilerT :: CompilerT m a -> m (Either CompilerError a)
runCompilerT = runExceptT . unCompilerT

-- | Run a concrete compiler computation in IO
runCompiler :: Compiler a -> IO (Either CompilerError a)
runCompiler = runCompilerT

-- | Lift Either Text values into compiler errors
liftCompilerEither :: MonadError CompilerError m => Either Text a -> m a
liftCompilerEither (Left err) = throwError $ CodegenError err
liftCompilerEither (Right x) = return x

-- | Convert Maybe values into compiler errors
noteCompilerError :: MonadError CompilerError m => CompilerError -> Maybe a -> m a
noteCompilerError err Nothing = throwError err
noteCompilerError _ (Just x) = return x

-- | Convert lexer errors from Megaparsec into compiler errors.
-- Used primarily in the lexing stage of compilation.
liftLexResult :: MonadError CompilerError m 
              => Either (M.ParseErrorBundle Text Tokens.CLexError) a 
              -> m a
liftLexResult = either (throwError . LexerError) return

-- | Convert parser errors from Megaparsec into compiler errors.
-- Used primarily in the parsing stage of compilation.
liftParseResult :: MonadError CompilerError m 
                => Either (M.ParseErrorBundle [Tokens.CToken] Tokens.CParseError) a 
                -> m a
liftParseResult = either (throwError . ParserError) return

-- | Format a compiler error for human consumption
formatError :: CompilerError -> Text
formatError = \case
  LexerError e -> T.pack $ M.errorBundlePretty e
  ParserError e -> T.pack $ M.errorBundlePretty e
  CodegenError e -> e
  IOError e -> e
  SystemError e -> e

-- | Convert a compiler error to a structured form for testing
toTestError :: CompilerError -> Text
toTestError = \case
  LexerError _ -> "lexer_error"
  ParserError _ -> "parser_error"
  CodegenError _ -> "codegen_error"
  IOError _ -> "io_error"
  SystemError _ -> "system_error"