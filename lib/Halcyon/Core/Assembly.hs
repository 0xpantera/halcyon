module Halcyon.Core.Assembly where

import Data.Text ( Text )


data Program = Program Function
  deriving (Eq, Show)

data Function = Function 
  { name         :: Text
  , instructions :: [Instruction]
  } deriving (Eq, Show)

data Instruction
  = Mov { src :: Operand, dst :: Operand }
  | Unary { operator :: UnaryOp, operand :: Operand }
  | AllocateStack { bytes :: Int }
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