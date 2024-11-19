{-# LANGUAGE RecordWildCards #-}
module Halcyon.Backend.Codegen where

import qualified Halcyon.Core.Assembly as Asm
import qualified Halcyon.Core.Tacky as Tacky

convertVal :: Tacky.TackyVal -> Asm.Operand
convertVal (Tacky.Constant int)   = Asm.Imm int
convertVal (Tacky.Var identifier) = Asm.Pseudo identifier

convertOp :: Tacky.UnaryOp -> Asm.UnaryOp
convertOp Tacky.Complement = Asm.Not
convertOp Tacky.Negate     = Asm.Neg

convertInstruction :: Tacky.Instruction -> [Asm.Instruction]
convertInstruction (Tacky.Return val) =
  let 
    asmVal = convertVal val
    asmReg = Asm.Register Asm.Ax
  in [Asm.Mov asmVal asmReg, Asm.Ret]
convertInstruction (Tacky.Unary op src dest) =
  let 
    asmSrc  = convertVal src
    asmDest = convertVal dest
    asmOp   = convertOp op
  in [Asm.Mov asmSrc asmDest, Asm.Unary asmOp asmDest]

convertFunction :: Tacky.FunctionDef -> Asm.FunctionDef
convertFunction Tacky.Function{..} =
  let instructions = concatMap convertInstruction body
  in Asm.Function {name, instructions}

gen :: Tacky.Program -> Asm.Program
gen (Tacky.Program fnDef) = Asm.Program $ convertFunction fnDef
