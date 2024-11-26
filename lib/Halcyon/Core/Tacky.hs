module Halcyon.Core.Tacky where

import Data.Text ( Text )

data Program = Program Function
  deriving (Eq, Show)

data Function = Function Text [Instruction]
  deriving (Eq, Show)

data Instruction
  = Return Val
  | Unary UnaryOp Val Val 
  deriving (Eq, Show)

data Val
  = Constant Int
  | Var Text
  deriving (Eq, Show)

data UnaryOp
  = Complement
  | Negate
  deriving (Eq, Show)

--program = Program(function_definition)
--function_definition = Function(identifier, instruction* body)
--instruction = Return(val) | Unary(unary_operator, val src, val dst)
--val = Constant(int) | Var(identifier)
--unary_operator = Complement | Negate