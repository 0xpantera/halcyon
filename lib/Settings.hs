module Settings where


data Stage = 
    Lex
  | Parse
  | Codegen
  | Assembly
  | Executable
  deriving (Eq, Show)

data Target = OSX | Linux
    deriving (Eq, Show)
