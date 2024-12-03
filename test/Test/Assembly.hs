{-# LANGUAGE OverloadedStrings #-}
module Test.Assembly (assemblySpecs) where

import Test.Hspec
import Halcyon.Backend (gen)
import Halcyon.Core.Assembly qualified as Asm
import Halcyon.Core.Tacky qualified as Tacky

assemblySpecs :: Spec
assemblySpecs = describe "Assembly Generation" $ do
  it "generates assembly for simple return" $
    let tacky = Tacky.Program $ Tacky.Function "main" [Tacky.Return (Tacky.Constant 42)]
    in gen tacky `shouldBe` Asm.Program (Asm.Function "main" 
         [ Asm.Mov (Asm.Imm 42) (Asm.Register Asm.Ax)
         , Asm.Ret])

  it "generates assembly for unary operation" $
    let tacky = Tacky.Program $ Tacky.Function "main" 
          [ Tacky.Unary Tacky.Negate (Tacky.Constant 42) (Tacky.Var "tmp.0")
          , Tacky.Return (Tacky.Var "tmp.0")]
    in gen tacky `shouldBe` Asm.Program (Asm.Function "main"
         [ Asm.Mov (Asm.Imm 42) (Asm.Pseudo "tmp.0")
         , Asm.Unary Asm.Neg (Asm.Pseudo "tmp.0")
         , Asm.Mov (Asm.Pseudo "tmp.0") (Asm.Register Asm.Ax)
         , Asm.Ret])