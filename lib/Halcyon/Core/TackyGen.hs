{-#LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Halcyon.Core.TackyGen 
( genTacky
) where

import Control.Monad.State
import Data.Text (Text)
import qualified Data.Text as T

import Halcyon.Core.Ast qualified as Ast  
import Halcyon.Core.Tacky qualified as Tacky
import Halcyon.Core.Monad (MonadCompiler)

newtype TackyGenT m a = TackyGenT 
  { runTackyGenT :: StateT Int m a }
  deriving (Functor, Applicative, Monad, MonadState Int)

data TackyGenRes = TackyGenRes
  { instructions :: [Tacky.Instruction]
  , resultVal :: Tacky.TackyVal 
  } deriving (Show, Eq)

-- Helper to generate unique names
freshTemp :: Monad m => TackyGenT m Text
freshTemp = do
  n <- get
  put (n + 1)
  pure $ "tmp." <> T.pack (show n)

convertOp :: Ast.UnaryOp -> Tacky.UnaryOp
convertOp Ast.Complement = Tacky.Complement
convertOp Ast.Negate     = Tacky.Negate

emitTackyForExpr :: Monad m => Ast.Expr -> TackyGenT m TackyGenRes
emitTackyForExpr = \case
  Ast.Constant c -> pure $ TackyGenRes 
    { instructions = [], resultVal = Tacky.Constant c }
  Ast.Unary op inner -> do
    dstName <- freshTemp
    TackyGenRes{..} <- emitTackyForExpr inner
    let 
      dst = Tacky.Var dstName
      tackyOp = convertOp op
      newInstr = Tacky.Unary tackyOp resultVal dst
    pure $ TackyGenRes
      { instructions = instructions ++ [newInstr], resultVal = dst }
        
emitTackyForStmnt :: Monad m => Ast.Statement -> TackyGenT m TackyGenRes
emitTackyForStmnt (Ast.Return e) = do
  TackyGenRes{..} <- emitTackyForExpr e
  pure $ TackyGenRes {..}

emitTackyForFunc :: Monad m => Ast.FunctionDef -> TackyGenT m Tacky.FunctionDef
emitTackyForFunc Ast.Function{..} = do
  TackyGenRes{..} <- emitTackyForStmnt body
  let finalInstrs = instructions ++ [Tacky.Return resultVal]
  pure $ Tacky.Function {name, body = finalInstrs}

emitTackyProgram :: Monad m => Ast.Program -> TackyGenT m Tacky.Program
emitTackyProgram (Ast.Program fnDef) = do
  tackyFunc <- emitTackyForFunc fnDef
  pure $ Tacky.Program tackyFunc

genTacky :: MonadCompiler m => Ast.Program -> m Tacky.Program 
genTacky program = 
  evalStateT (runTackyGenT $ emitTackyProgram program) 0
