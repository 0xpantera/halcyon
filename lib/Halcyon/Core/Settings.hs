module Halcyon.Core.Settings where

import Data.Text (Text)

import Halcyon.Frontend.Tokens (CToken)
import qualified Halcyon.Core.Ast as Ast
import qualified Halcyon.Core.Assembly as Asm
import qualified Halcyon.Core.Tacky as Tacky


data Stage 
  = Lex
  | Parse
  | Tacky
  | Codegen
  | Assembly
  | Executable
  deriving (Eq, Show)

data Target = OSX | Linux
    deriving (Eq, Show)

data StageResult
  = StageResultTokens [CToken]
  | StageResultAST Ast.Program
  | StageResultTacky Tacky.Program
  | StageResultAsm Asm.Program
  | StageResultAssembly Text
  | StageResultExecutable Text
  deriving (Show)