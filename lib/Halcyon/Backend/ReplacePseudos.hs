{-# LANGUAGE RecordWildCards #-}
module Halcyon.Backend.ReplacePseudos where

import Data.Traversable (mapAccumL)
import qualified Data.Map.Strict as Map
import Data.Text ( Text )

import qualified Halcyon.Core.Assembly as Asm

-- Structure to keep track of what stack slots we've assigned so far
data ReplacementSt = ReplacementSt
  { currentOffset :: Int              -- last used stack slot
  , offsetMap     :: Map.Map Text Int -- map from pseudoregister to stack slots
  } deriving Show

replaceOperand :: ReplacementSt -> Asm.Operand -> (ReplacementSt, Asm.Operand)
replaceOperand state (Asm.Pseudo s) = case Map.lookup s (offsetMap state) of
  Just offset -> (state, Asm.Stack offset)
  Nothing -> 
    let 
      newOffset = (currentOffset state) - 4
      newMap = Map.insert s newOffset (offsetMap state)
      newState = ReplacementSt {
        currentOffset = newOffset,
        offsetMap = newMap
      }
    in (newState, Asm.Stack newOffset)
replaceOperand state other = (state, other)

replacePseudoInst :: ReplacementSt -> Asm.Instruction -> (ReplacementSt, Asm.Instruction)
replacePseudoInst state (Asm.Mov src dst) =
  let 
    (state1, newSrc) = replaceOperand state src
    (state2, newDst) = replaceOperand state1 dst
    newMov = Asm.Mov newSrc newDst
    in (state2, newMov)
replacePseudoInst state (Asm.Unary op dst) =
  let
    (state1, newDst) = replaceOperand state dst
    newUnary = Asm.Unary op newDst
  in (state1, newUnary)
replacePseudoInst state Asm.Ret = (state, Asm.Ret)
replacePseudoInst state (Asm.AllocateStack _) = (state, Asm.AllocateStack 900)

replacePseudoFunc :: Asm.FunctionDef -> (ReplacementSt, Asm.FunctionDef)
replacePseudoFunc func@Asm.Function{..} =
  let 
    initState = ReplacementSt { currentOffset = 0, offsetMap = Map.empty}
    (finalState, fixedInstructions) = 
      mapAccumL replacePseudoInst initState instructions

  in (finalState, func {Asm.instructions = fixedInstructions})

replacePseudos :: Asm.Program -> (Int, Asm.Program)
replacePseudos (Asm.Program f) =
  let (lastState, fixedF) = replacePseudoFunc f
  in (currentOffset lastState, Asm.Program fixedF)