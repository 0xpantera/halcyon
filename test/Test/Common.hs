module Test.Common where

import Halcyon.Core.Ast as Ast

-- Smart constructors for building test cases
makeMainFunction :: Statement -> Program
makeMainFunction stmt = Program (Function "main" stmt)

makeReturn :: Expr -> Statement 
makeReturn = Return

makeConstant :: Int -> Expr
makeConstant = Constant

makeUnary :: UnaryOp -> Int -> Expr
makeUnary op n = Unary op (Constant n)

-- Combining them for common cases
simpleProgram :: Int -> Program
simpleProgram n = makeMainFunction (makeReturn (makeConstant n))

unaryProgram :: UnaryOp -> Int -> Program
unaryProgram op n = makeMainFunction (makeReturn (makeUnary op n))