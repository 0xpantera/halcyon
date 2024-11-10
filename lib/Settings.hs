module Settings where

import Data.Text (Text)

import Tokens (CToken)
import qualified Ast
import qualified Assembly as Asm


data Stage = 
    Lex
  | Parse
  | Codegen
  | Assembly
  | Executable
  deriving (Eq, Show)

data Target = OSX | Linux
    deriving (Eq, Show)

data StageResult
  = StageResultTokens [CToken]
  | StageResultAST Ast.Program
  | StageResultAsm Asm.Program
  | StageResultAssembly Text
  | StageResultExecutable Text
  deriving (Show)