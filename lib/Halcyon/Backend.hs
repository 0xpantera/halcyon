{-# LANGUAGE OverloadedStrings #-}
module Halcyon.Backend
  ( -- * Code Generation
    gen  -- from Codegen  
    -- * Assembly Output 
  , emitProgram -- from Emit
    -- * Register/Stack Management
  , replacePseudos
  , fixupProgram
  , ReplacePseudosError(..)
  , PseudoM
  ) where

import Halcyon.Backend.Codegen (gen)
import Halcyon.Backend.Emit (emitProgram)
import Halcyon.Backend.ReplacePseudos 
  ( replacePseudos
  , fixupProgram
  , ReplacePseudosError(..)
  , PseudoM
  )