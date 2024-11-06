module Settings where


data Stage = 
    Lex
  | Parse
  | Validate
  | Tacky
  | Codegen
  | Assembly
  | Obj
  | Executable
  deriving (Eq, Show)

data Target = OSX | Linux
    deriving (Eq, Show)
