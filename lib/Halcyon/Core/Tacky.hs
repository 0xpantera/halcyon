module Halcyon.Core.Tacky where

import Data.Text ( Text )

data Program = Program FunctionDef
  deriving (Eq, Show)

data FunctionDef = Function
  { name :: Text
  , body :: [Instruction]
  } deriving (Eq, Show)

data Instruction 
  = Return TackyVal
  | Unary UnaryOp TackyVal TackyVal
  deriving (Eq, Show)

data TackyVal
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