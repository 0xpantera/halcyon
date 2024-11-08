{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Emit where

import qualified Assembly as Asm
import Data.Text
import qualified Data.Text as T

--let show_operand = function
--  | Register -> "%eax"
--  | Imm i -> Printf.sprintf "$%d" i

showOperand :: Asm.Operand -> Text
showOperand Asm.Register = "%eax"
showOperand (Asm.Imm i) = "$" <> T.pack (show i)

--let show_label name =
--  match !Settings.platform with OS_X -> "_" ^ name | Linux -> name

--let emit_instruction chan = function
--  | Mov (src, dst) ->
--      Printf.fprintf chan "\tmovl %s, %s\n" (show_operand src)
--        (show_operand dst)
--  | Ret -> Printf.fprintf chan "\tret\n"

emitInstruction :: Asm.Instruction -> Text
emitInstruction (Asm.Mov src dest) =
  "\tmovl " <> showOperand src <> showOperand dest <> "\n"
emitInstruction Asm.Ret =
  "\tret\n"


--let emit_function chan (Function { name; instructions }) =
--  let label = show_label name in
--  Printf.fprintf chan {|
--  .globl %s
-- %s:
-- |} label label;
--  List.iter (emit_instruction chan) instructions

emitFunction :: Asm.FunctionDef -> Text
emitFunction Asm.Function{..} =
  let
    label = "_main"
    header = ".globl " <> label <> "\n" <>
      label <> ":\n"
    instructionsTxt = T.concat $ Prelude.map emitInstruction instructions
  in
    header <> instructionsTxt 

--let emit assembly_file (Program function_def) =
--  let output_channel = open_out assembly_file in
--  emit_function output_channel function_def;
--  emit_stack_note output_channel;
--  close_out output_channel

emitProgram :: Asm.Program -> Text
emitProgram (Asm.Program func) =
  emitFunction func