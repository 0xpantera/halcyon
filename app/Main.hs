module Main where

import Cli (parseOptions)
import Pipeline (processFile)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr, stdout)

main :: IO ()
main = do
  -- Debug logging of command line args
  args <- getArgs
  let debugMsg = "Args: " ++ show args ++ "\n"
  hPutStrLn stdout debugMsg
  
  -- Parse options and log them
  opts <- parseOptions
  let optionsMsg = "Parsed options: " ++ show opts ++ "\n"
  hPutStrLn stderr optionsMsg
  
  -- Run the compiler pipeline
  processFile opts