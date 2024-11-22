module Halcyon.Core.Assembly where

import Data.Text ( Text )

--TODO: use record syntax to name some of the product types
-- e.i. Mov Operand Operand -> Mov {src: Operand, dest: Operand}
data Program = Program FunctionDef
  deriving (Eq, Show)

data FunctionDef = Function 
  { name         :: Text
  , instructions :: [Instruction]
  } deriving (Eq, Show)

data Instruction 
  = Mov Operand Operand
  | Unary UnaryOp Operand
  | AllocateStack Int
  | Ret
  deriving (Eq, Show)

data UnaryOp = Neg | Not
  deriving (Eq, Show)

data Operand 
  = Imm Int 
  | Register Reg
  | Pseudo Text
  | Stack Int
  deriving (Eq, Show)

data Reg = Ax | R10
  deriving (Eq, Show)

{-
program = Program(function_definition)
function_definition = Function(identifier name, instruction* instructions)
instruction 
  = Mov(operand src, operand dst)
  | Unary(unary_operator, operand)
  | AllocateStack(int)
  | Ret
unary_operator = Neg | Not
operand 
  = Imm(int) 
  | Reg(reg) 
  | Pseudo(identifier) 
  | Stack(int)
reg = AX | R10
-}