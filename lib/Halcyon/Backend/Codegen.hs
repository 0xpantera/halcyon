{-# LANGUAGE RecordWildCards #-}
module Halcyon.Backend.Codegen where

import qualified Halcyon.Core.Assembly as Asm
import qualified Halcyon.Core.Tacky as Tacky

convertVal :: Tacky.Val -> Asm.Operand
convertVal (Tacky.Constant int)   = Asm.Imm int
convertVal (Tacky.Var identifier) = Asm.Pseudo identifier

convertUnaryOp :: Tacky.UnaryOp -> Asm.UnaryOp
convertUnaryOp Tacky.Complement = Asm.Not
convertUnaryOp Tacky.Negate     = Asm.Neg

convertBinaryOp :: Tacky.BinaryOp -> Maybe Asm.BinaryOp
convertBinaryOp = \case
  Tacky.Add -> Just Asm.Add
  Tacky.Subtract -> Just Asm.Sub  
  Tacky.Multiply -> Just Asm.Mult
  _ -> Nothing -- Division and Remainder don't map directly

-- For regular binary operations (add, sub, mult)
genRegularBinary :: Asm.BinaryOp -> Asm.Operand -> Asm.Operand -> Asm.Operand -> [Asm.Instruction]
genRegularBinary op src1 src2 dst =
  [ Asm.Mov src1 dst
  , Asm.Binary op src2 dst
  ]

-- For division/remainder operations
genDivision :: Bool -> Asm.Operand -> Asm.Operand -> Asm.Operand -> [Asm.Instruction]
genDivision isRemainder src1 src2 dst =
  [ Asm.Mov src1 (Asm.Register Asm.Ax)
  , Asm.Cdq
  , Asm.Idiv src2 
  , Asm.Mov (Asm.Register (if isRemainder then Asm.DX else Asm.Ax)) dst
  ]

convertInstruction :: Tacky.Instruction -> [Asm.Instruction]
convertInstruction = \case
  Tacky.Return val ->
    let 
      asmVal = convertVal val
      asmReg = Asm.Register Asm.Ax
    in [Asm.Mov asmVal asmReg, Asm.Ret]
  Tacky.Unary op src dst ->
    let 
      asmSrc  = convertVal src
      asmDst = convertVal dst
      asmOp   = convertUnaryOp op
    in [Asm.Mov asmSrc asmDst, Asm.Unary asmOp asmDst]
  Tacky.Binary op src1 src2 dst ->
    let
      asmSrc1 = convertVal src1
      asmSrc2 = convertVal src2
      asmDst  = convertVal dst
      in case op of
        Tacky.Divide ->
          genDivision False asmSrc1 asmSrc2 asmDst
        Tacky.Remainder ->
          genDivision True asmSrc1 asmSrc2 asmDst
        _ -> case convertBinaryOp op of
          Just asmOp -> genRegularBinary asmOp asmSrc1 asmSrc2 asmDst
          Nothing -> error $ "Unreachable unsupported binary operator: " ++ show op

convertFunction :: Tacky.Function -> Asm.Function
convertFunction (Tacky.Function name body) =
  let instructions = concatMap convertInstruction body
  in Asm.Function name instructions

gen :: Tacky.Program -> Asm.Program
gen (Tacky.Program fnDef) = Asm.Program $ convertFunction fnDef
