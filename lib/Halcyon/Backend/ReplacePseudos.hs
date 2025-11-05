{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Halcyon.Backend.ReplacePseudos
  ( replacePseudos,
    fixupProgram,
    ReplacePseudosError (..),
    PseudoM,
  )
where

import Control.Monad.Except
import Control.Monad.State
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Halcyon.Core.Assembly

-- Structure to keep track of what stack slots we've assigned so far
data ReplacementSt = ReplacementSt
  { currentOffset :: Int, -- last used stack slot
    offsetMap :: Map.Map Text Int -- map from pseudoregister to stack slots
  }
  deriving (Show)

data ReplacePseudosError
  = UnexpectedAllocateStack
  deriving (Show, Eq)

type PseudoM = Either ReplacePseudosError

-- | State monad transformer that can also fail with ReplacePseudosError
-- type ReplaceM = StateT ReplacementSt PseudoM

-- | Constraint for monads that can perform compiler operations
type MonadReplace m =
  ( MonadState ReplacementSt m,
    MonadError ReplacePseudosError m
  )

replaceOperand :: (MonadReplace m) => Operand -> m Operand
replaceOperand (Pseudo s) = do
  st <- get
  case Map.lookup s st.offsetMap of
    Just offset -> pure $ Stack offset
    Nothing -> do
      let newOffset = st.currentOffset - 4
          newMap = Map.insert s newOffset st.offsetMap
      put $
        ReplacementSt
          { currentOffset = newOffset,
            offsetMap = newMap
          }
      pure $ Stack newOffset
replaceOperand other = pure other

replacePseudoInst :: (MonadReplace m) => Instruction -> m Instruction
replacePseudoInst = \case
  Mov src dst -> do
    newSrc <- replaceOperand src
    newDst <- replaceOperand dst
    pure $ Mov newSrc newDst
  Unary op dst -> do
    newDst <- replaceOperand dst
    pure $ Unary op newDst
  Binary op src dst -> do
    newSrc <- replaceOperand src
    newDst <- replaceOperand dst
    pure $ Binary op newSrc newDst
  Idiv operand -> do
    newOperand <- replaceOperand operand
    pure $ Idiv newOperand
  Cdq -> pure Cdq
  Ret -> pure Ret
  AllocateStack _ -> throwError UnexpectedAllocateStack

replacePseudoFunc :: (MonadReplace m) => Function -> m Function
replacePseudoFunc (Function name instructions) = do
  fixedInstructions <- traverse replacePseudoInst instructions
  pure $ Function name fixedInstructions

replacePseudos :: Program -> PseudoM (Program, Int)
replacePseudos (Program f) = do
  let initState =
        ReplacementSt
          { currentOffset = 0,
            offsetMap = Map.empty
          }
  (fixedF, finalState) <- runStateT (replacePseudoFunc f) initState
  pure (Program fixedF, finalState.currentOffset)

-- Helper functions to construct common instructions
movToR10 :: Operand -> Instruction
movToR10 src = Mov src (Register R10)

movFromR10 :: Operand -> Instruction
movFromR10 dst = Mov (Register R10) dst

movToR11 :: Operand -> Instruction
movToR11 src = Mov src (Register R11)

movFromR11 :: Operand -> Instruction
movFromR11 dst = Mov (Register R11) dst

-- Fixes memory-to-memory moves
fixMemoryMove :: Int -> Int -> [Instruction]
fixMemoryMove src dst =
  [ movToR10 (Stack src),
    movFromR10 (Stack dst)
  ]

-- Fixes immediate operand in idiv
fixImmediateIdiv :: Int -> [Instruction]
fixImmediateIdiv n =
  [ movToR10 (Imm n),
    Idiv (Register R10)
  ]

-- Fixes memory operands in Add/Sub
fixMemoryBinary :: BinaryOp -> Int -> Int -> [Instruction]
fixMemoryBinary op src dst =
  [ movToR10 (Stack src),
    Binary op (Register R10) (Stack dst)
  ]

-- Fixes memory destination in Mult
fixMemoryMult :: Operand -> Int -> [Instruction]
fixMemoryMult src dst =
  [ movToR11 (Stack dst),
    Binary Mult src (Register R11),
    movFromR11 (Stack dst)
  ]

fixupInstruction :: Instruction -> [Instruction]
fixupInstruction = \case
  -- Memory-to-memory move
  Mov (Stack src) (Stack dst) ->
    fixMemoryMove src dst
  -- Immediate operand in idiv
  Idiv (Imm n) ->
    fixImmediateIdiv n
  -- Memory operands in Add/Sub
  Binary op (Stack src) (Stack dst)
    | op `elem` [Add, Sub] ->
        fixMemoryBinary op src dst
  -- Memory destination in Mult
  Binary Mult src (Stack dst) ->
    fixMemoryMult src dst
  -- No fixup needed
  other -> [other]

fixupFunction :: Function -> Int -> Function
fixupFunction (Function name instructions) lastStackSlot =
  let fixedInstructions =
        (AllocateStack lastStackSlot)
          : concatMap fixupInstruction instructions
   in Function name $ fixedInstructions

fixupProgram :: Program -> Int -> Program
fixupProgram (Program f) lastStackSlot =
  Program (fixupFunction f lastStackSlot)
