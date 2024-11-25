module Halcyon.Core.Ast where

import Data.Text ( Text )

data Program = Program Function
  deriving (Eq, Show)

data Function = Function 
  { name :: Text
  , body :: Statement
  } deriving (Eq, Show)

data Statement = Return Expr
  deriving (Eq, Show)

data Expr
  = Constant { value :: Int }
  | Unary { operator :: UnaryOp, operand :: Expr }
  deriving (Eq, Show)

data UnaryOp = Complement | Negate
  deriving (Eq, Show)

-- Formal Grammar in EBNF
-- <program> ::= <function>
-- <function> ::= "int" <identifier> "(" "void" ")" "{" <statement> "}"
-- <statement> ::= "return" <exp> ";"
-- <exp> ::= <int> | <unop> <exp> | "(" <exp> ")"
-- <unop> ::= "-" | "~"
-- <identifier> ::= ? An identifier token ?
-- <int> ::= ? A constant token ?

-- AST in ASDL
-- program = Program(function_definition)
-- function_definition = Function(identifier name, statement body)
-- statement = Return(exp)
-- exp = Constant(int) | Unary(unary_op, exp)
-- unaryOp = Complement | Negate