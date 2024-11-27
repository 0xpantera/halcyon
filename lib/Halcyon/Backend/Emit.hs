{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Halcyon.Backend.Emit where

import Halcyon.Core.Assembly
import Data.Text ( Text )
import qualified Data.Text as T


showOperand :: Operand -> Text
showOperand = \case
  (Register Ax)  -> "%eax"
  (Register DX)  -> "%edx"
  (Register R10) -> "%r10d"
  (Register R11) -> "%r11d"
  (Imm i)        -> "$" <> T.pack (show i)
  (Stack i)      -> T.pack (show i) <> "(%rbp)"
  (Pseudo name)  -> "%" <> name

showUnaryOp :: UnaryOp -> Text
showUnaryOp = \case
  Not -> "notl"
  Neg -> "negl"

showBinaryOp :: BinaryOp -> Text
showBinaryOp = \case
  Add  -> "addl"
  Sub  -> "subl"
  Mult -> "imull"

--let show_label name =
--  match !Settings.platform with OS_X -> "_" ^ name | Linux -> name

emitInstruction :: Instruction -> Text
emitInstruction = \case
  (Mov src dest) ->
    "\tmovl " <> showOperand src <> ", " <> showOperand dest <> "\n"
  (Unary op operand) ->
    "\t" <> showUnaryOp op <> " " 
    <> showOperand operand <> "\n"
  (Binary op src dst) ->
    "\t" <> showBinaryOp op <> " " <> showOperand src <> ", " <> showOperand dst <> "\n"
  (Idiv operand) ->
    "\tidivl " <> showOperand operand <> "\n"
  Cdq -> 
    "\tcdq\n"
  (AllocateStack i) ->
    "\tsubq $" <> T.pack (show i) <> ", %rsp\n"
  Ret ->
    "\tmovq %rbp, %rsp\n\tpopq %rbp\n\tret\n"

emitFunction :: Function -> Text
emitFunction (Function _ instructions) =
  let
    header 
      =  "\t.globl _main\n" 
      <> "_main:\n" 
      <> "\tpushq %rbp\n" 
      <> "\tmovq %rsp, %rbp\n"
    instructionsTxt = T.concat $ map emitInstruction instructions
  in
    header <> instructionsTxt 

emitProgram :: Program -> Text
emitProgram (Program func) =
  emitFunction func