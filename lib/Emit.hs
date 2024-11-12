{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Emit where

import qualified Assembly as Asm
import Data.Text
import qualified Data.Text as T

showOperand :: Asm.Operand -> Text
showOperand Asm.Register = "%eax"
showOperand (Asm.Imm i) = "$" <> T.pack (show i)

--let show_label name =
--  match !Settings.platform with OS_X -> "_" ^ name | Linux -> name

emitInstruction :: Asm.Instruction -> Text
emitInstruction (Asm.Mov src dest) =
  "\tmovl " <> showOperand src <> ", " <> showOperand dest <> "\n"
emitInstruction Asm.Ret =
  "\tret\n"


emitFunction :: Asm.FunctionDef -> Text
emitFunction Asm.Function{..} =
  let
    label = "_main"
    header = ".globl " <> label <> "\n" <>
      label <> ":\n"
    instructionsTxt = T.concat $ Prelude.map emitInstruction instructions
  in
    header <> instructionsTxt 

emitProgram :: Asm.Program -> Text
emitProgram (Asm.Program func) =
  emitFunction func