module Halcyon.Core.Assembly where

import Data.Text ( Text )

-- AST ASDL
-- program = Program(function_definition)
-- function_definition = Function(identifier name, instruction* instructions)
-- instruction = Mov(operand src, operand dst) | Ret
-- operand = Imm(int) | Register

data Program = Program FunctionDef
  deriving (Eq, Show)

data FunctionDef = Function 
  { name         :: Text
  , instructions :: [Instruction]
  } deriving (Eq, Show)

data Instruction = Mov Operand Operand | Ret
  deriving (Eq, Show)

data Operand = Imm Int | Register
  deriving (Eq, Show)
