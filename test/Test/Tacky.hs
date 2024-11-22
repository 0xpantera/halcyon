{-# LANGUAGE OverloadedStrings #-}
module Test.Tacky (tackySpecs) where

import Test.Hspec
import Halcyon.Core.TackyGen (genTacky)
import Halcyon.Core.Ast qualified as Ast
import Halcyon.Core.Tacky qualified as Tacky
import Halcyon.Core.Monad (runCompiler)

tackySpecs :: Spec
tackySpecs = describe "TACKY Generation" $ do
  it "generates TACKY for simple return" $ do
    let ast = Ast.Program (Ast.Function "main" (Ast.Return (Ast.Constant 42)))
    result <- runCompiler $ genTacky ast
    result `shouldBe` Right (Tacky.Program $ Tacky.Function "main" [Tacky.Return (Tacky.Constant 42)])

  it "generates TACKY for unary operation" $ do
    let ast = Ast.Program (Ast.Function "main" (Ast.Return (Ast.Unary Ast.Negate (Ast.Constant 42))))
    result <- runCompiler $ genTacky ast
    result `shouldBe` Right (Tacky.Program $ Tacky.Function "main" 
      [ Tacky.Unary Tacky.Negate (Tacky.Constant 42) (Tacky.Var "tmp.0")
      , Tacky.Return (Tacky.Var "tmp.0")])