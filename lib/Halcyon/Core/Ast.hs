module Halcyon.Core.Ast where

import Data.Text ( Text )

data Program = Program FunctionDef
  deriving Show

data FunctionDef = Function 
  { name :: Text
  , body :: Statement
  } deriving Show

data Statement = Return Expr
  deriving Show

data Expr = Constant Int
  deriving Show

-- Formal Grammar in EBNF
-- <program> ::= <function>
-- <function> ::= "int" <identifier> "(" "void" ")" "{" <statement> "}"
-- <statement> ::= "return" <exp> ";"
-- <exp> ::= <int>
-- <identifier> ::= ? An identifier token ?
-- <int> ::= ? A constant token ?

-- AST in ASDL
--program = Program(function_definition)
--function_definition = Function(identifier name, statement body)
--statement = Return(exp)
--exp = Constant(int)