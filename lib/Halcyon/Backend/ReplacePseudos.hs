{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Halcyon.Backend.ReplacePseudos 
  ( replacePseudos
  , fixupProgram
  , ReplacePseudosError(..)
  , PseudoM
  ) where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map.Strict as Map
import Data.Text ( Text )

import qualified Halcyon.Core.Assembly as Asm

-- Structure to keep track of what stack slots we've assigned so far
data ReplacementSt = ReplacementSt
  { currentOffset :: Int              -- last used stack slot
  , offsetMap     :: Map.Map Text Int -- map from pseudoregister to stack slots
  } deriving Show

data ReplacePseudosError 
  = UnexpectedAllocateStack
  deriving (Show, Eq)

type PseudoM = Either ReplacePseudosError

-- | State monad transformer that can also fail with ReplacePseudosError
type ReplaceM = StateT ReplacementSt PseudoM

-- | Constraint for monads that can perform compiler operations
type MonadReplace m = 
  ( MonadState ReplacementSt m
  , MonadError ReplacePseudosError m
  )

replaceOperand :: MonadReplace m => Asm.Operand -> m Asm.Operand
replaceOperand (Asm.Pseudo s) = do
  st <- get
  case Map.lookup s st.offsetMap of
    Just offset -> pure $ Asm.Stack offset
    Nothing -> do
      let 
        newOffset =  st.currentOffset - 4
        newMap = Map.insert s newOffset st.offsetMap
      put $ ReplacementSt {
        currentOffset = newOffset,
        offsetMap = newMap
      }
      pure $ Asm.Stack newOffset
replaceOperand other = pure other

replacePseudoInst :: MonadReplace m => Asm.Instruction -> m Asm.Instruction
replacePseudoInst = \case
  Asm.Mov src dst -> do
    newSrc <- replaceOperand src
    newDst <- replaceOperand dst
    pure $ Asm.Mov newSrc newDst
  Asm.Unary op dst -> do
    newDst <- replaceOperand dst
    pure $ Asm.Unary op newDst
  Asm.Ret -> pure Asm.Ret
  Asm.AllocateStack _ -> throwError UnexpectedAllocateStack

replacePseudoFunc :: MonadReplace m => Asm.Function -> m Asm.Function
replacePseudoFunc func@Asm.Function{..} = do
  fixedInstructions <- traverse replacePseudoInst instructions
  pure $ func { Asm.instructions = fixedInstructions }

replacePseudos :: Asm.Program -> PseudoM (Asm.Program, Int)
replacePseudos (Asm.Program f) = do
  let initState = ReplacementSt 
        { currentOffset = 0
        , offsetMap = Map.empty
        }
  -- Run the stateful computation and get both result and final state
  (fixedF, finalState) <- runStateT (replacePseudoFunc f) initState
  -- Return the program and just the offset we need
  pure (Asm.Program fixedF, currentOffset finalState)

fixupInstruction :: Asm.Instruction -> [Asm.Instruction]
fixupInstruction (Asm.Mov (Asm.Stack src) (Asm.Stack dst)) = 
  [ (Asm.Mov (Asm.Stack src) (Asm.Register Asm.R10)) 
  , (Asm.Mov (Asm.Register Asm.R10) (Asm.Stack dst)) ]
fixupInstruction other = [ other ]

fixupFunction :: Asm.Function -> Int -> Asm.Function
fixupFunction (Asm.Function{..}) lastStackSlot = Asm.Function 
  { name
  , instructions = 
      (Asm.AllocateStack lastStackSlot) 
        : concatMap fixupInstruction instructions
  }

fixupProgram :: Asm.Program -> Int -> Asm.Program
fixupProgram (Asm.Program f) lastStackSlot =
  Asm.Program (fixupFunction f lastStackSlot) 
