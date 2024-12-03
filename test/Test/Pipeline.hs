{-#LANGUAGE OverloadedStrings #-}
module Test.Pipeline (pipelineSpecs) where

import Test.Hspec

import Halcyon.Backend (gen)
import Halcyon.Core.Assembly
import Halcyon.Core (genTacky, runCompiler, liftLexResult, liftParseResult)
import Halcyon.Frontend (parseTokens, lexer)
-- import Halcyon.Core.Monad
import Text.Megaparsec (parse)

pipelineSpecs :: Spec
pipelineSpecs = describe "Compilation Pipeline" $ do
  it "compiles constant expression" $ do
    let source = "int main(void) { return 42; }"
    result <- runCompiler $ do
      tokens <- liftLexResult $ parse lexer "" source
      ast <- liftParseResult $ parseTokens tokens
      tacky <- genTacky ast
      pure $ gen tacky
    result `shouldBe` Right (Program (Function "main" [Mov (Imm 42) (Register Ax), Ret]))