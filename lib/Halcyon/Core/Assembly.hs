module Halcyon.Core.Assembly where

import Data.Text ( Text )


data Program = Program Function
  deriving (Eq, Show)

data Function = Function Text [Instruction]
  deriving (Eq, Show)

data Instruction
  = Mov Operand Operand
  | Unary UnaryOp Operand
  | Binary BinaryOp Operand Operand
  | Idiv Operand
  | Cdq
  | AllocateStack Int
  | Ret
  deriving (Eq, Show)

data UnaryOp = Neg | Not
  deriving (Eq, Show)

data BinaryOp = Add | Sub | Mult
  deriving (Eq, Show)

data Operand 
  = Imm Int 
  | Register Reg
  | Pseudo Text
  | Stack Int
  deriving (Eq, Show)

data Reg = Ax | DX | R10 | R11
  deriving (Eq, Show)

{-
program = Program(function_definition)
function_definition = Function(identifier name, instruction* instructions)
instruction 
  = Mov(operand src, operand dst)
  | Unary(unary_operator, operand)
  | Binary(binary_operator, operand, operand)
  | Idiv(operand)
  | Cdq
  | AllocateStack(int)
  | Ret
unary_operator = Neg | Not
binary_operator = Add | Sub | Mult
operand = Imm(int) | Reg(reg) | Pseudo(identifier) | Stack(int)
reg = AX | DX | R10 | R11
-}