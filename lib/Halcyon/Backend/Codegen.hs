{-# LANGUAGE RecordWildCards #-}
module Halcyon.Backend.Codegen where

import qualified Halcyon.Core.Assembly as Asm
import qualified Halcyon.Core.Ast as Ast

import Data.Text ( Text )

convertExp :: Ast.Expr -> Either Text Asm.Operand
convertExp (Ast.Constant i) = Right $ Asm.Imm i

convertStatement :: Ast.Statement -> Either Text [Asm.Instruction]
convertStatement (Ast.Return e) = do
  v <- convertExp e
  pure [Asm.Mov v Asm.Register, Asm.Ret]

convertFunction :: Ast.FunctionDef -> Either Text Asm.FunctionDef
convertFunction Ast.Function{..} = do
  instructions <- convertStatement body
  pure Asm.Function {name, instructions}

gen :: Ast.Program -> Either Text Asm.Program
gen (Ast.Program fnDef) = Asm.Program <$> convertFunction fnDef
