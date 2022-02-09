module Lexer where

import Lambda 

parseToExpression :: String -> Expression
parseToExpression = parseToExpression' NoneState

data ParseState = VarState | AbsState | AppState | NoneState

parseToExpression' :: ParseState -> String -> Expression
parseToExpression' NoneState (x:xs) = error ""
     
