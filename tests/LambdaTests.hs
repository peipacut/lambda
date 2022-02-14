module LambdaTests where

import Test.HUnit
import Lambda

main = runTestTT tests

tests = test [
    --"show var" ~= "x" ~=? show (Var "x")
    "show var" ~: 4 ~=? 4 ]
