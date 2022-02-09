module Lexer where

import Lambda 

data StackAlphabet = Z | Open
data State = StartS | VarS | AbsS | AppS

parseToExpression :: String -> Expression
parseToExpression = pda StartS [Z] 

pda :: State -> [StackAlphabet] -> String -> Expression
pda state stack (x:xs) = Var "x"
pda _ _ _ = Var "x"
