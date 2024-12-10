module Halcyon.Core.Ast where

import Data.Text ( Text )

data Program = Program Function
  deriving (Eq, Show)

data Function = Function Text Statement
  deriving (Eq, Show)

data Statement = Return Expr
  deriving (Eq, Show)

data Expr
  = Constant Int
  | Unary UnaryOp Expr
  | Binary BinaryOp Expr Expr
  deriving (Eq, Show)

data UnaryOp = Complement | Negate | Not
  deriving (Eq, Show)

data BinaryOp = Add | Subtract | Multiply | Divide | Remainder
  | And | Or | Equal | NotEqual | LessThan | LessOrEqual
  | GreaterThan | GreaterOrEqual
  deriving (Eq, Show)


-- AST in ASDL
{-
program = Program(function_definition)
function_definition = Function(identifier name, statement body)
statement = Return(exp)
exp 
  = Constant(int)
  | Unary(unary_operator, exp)
  | Binary(binary_operator, exp, exp)
unary_operator = Complement | Negate | Not
binary_operator = Add | Subtract | Multiply | Divide | Remainder 
| And | Or | Equal | NotEqual | LessThan | LessOrEqual
| GreaterThan | GreaterOrEqual

-}

-- Formal Grammar in EBNF
{-
<program> ::= <function>
<function> ::= "int" <identifier> "(" "void" ")" "{" <statement> "}"
<statement> ::= "return" <exp> ";"
<exp> ::= <factor> | <exp> <binop> <exp>
<factor> ::= <int> | <unop> <factor> | "(" <exp> ")"
<unop> ::= "-" | "~"
<binop> ::= "-" | "+" | "*" | "/" | "%"
<identifier> ::= ? An identifier token ?
<int> ::= ? A constant token ?
-}