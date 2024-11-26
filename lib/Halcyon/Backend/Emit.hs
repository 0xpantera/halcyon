{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Halcyon.Backend.Emit where

import qualified Halcyon.Core.Assembly as Asm
import Data.Text ( Text )
import qualified Data.Text as T


showOperand :: Asm.Operand -> Text
showOperand (Asm.Register Asm.Ax) = "%eax"
showOperand (Asm.Register Asm.R10) = "%r10d"
showOperand (Asm.Imm i) = "$" <> T.pack (show i)
showOperand (Asm.Stack i) = T.pack (show i) <> "(%rbp)"
showOperand (Asm.Pseudo name) = "%" <> name

showOperator :: Asm.UnaryOp -> Text
showOperator Asm.Not = "notl"
showOperator Asm.Neg = "negl"

--let show_label name =
--  match !Settings.platform with OS_X -> "_" ^ name | Linux -> name

emitInstruction :: Asm.Instruction -> Text
emitInstruction (Asm.Mov src dest) =
  "\tmovl " <> showOperand src <> ", " <> showOperand dest <> "\n"
emitInstruction (Asm.AllocateStack i) = 
  "\tsubq $" <> T.pack (show i) <> ", %rsp\n"
emitInstruction (Asm.Unary op operand) = 
  "\t" <> showOperator op <> " " <> showOperand operand <> "\n"
emitInstruction Asm.Ret = 
  "\tmovq %rbp, %rsp\n\tpopq %rbp\n\tret\n"

emitFunction :: Asm.Function -> Text
emitFunction (Asm.Function _ instructions) =
  let
    header = 
      "\t.globl _main\n" <>
      "_main:\n" <>
      "\tpushq %rbp\n" <>
      "\tmovq %rsp, %rbp\n"
    instructionsTxt = T.concat $ map emitInstruction instructions
  in
    header <> instructionsTxt 

emitProgram :: Asm.Program -> Text
emitProgram (Asm.Program func) =
  emitFunction func