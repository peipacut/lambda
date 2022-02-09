module Interpreter where

import Lexer (parseToExpression)
import Lambda (toNormal)
import Control.Monad (when)

data Reduction = ToNormal | SingleStep
data EvaluationStrategy = Lazy | Eager

data InterpreterState = InterpreterState
    { reduction    :: Reduction
    , evalStrategy :: EvaluationStrategy
    }

initialState :: InterpreterState
initialState = InterpreterState
    { reduction = ToNormal
    , evalStrategy = Lazy
    }

main = do
    putStr "Lambda> "
    command <- getLine

    -- TODO: program state
    let (newState, changed) = handleState command initialState

    when changed
        main

    let expression  = parseToExpression command
        output      = toNormal expression

    print output
    main

handleState :: String -> InterpreterState -> (InterpreterState, Bool)
handleState args state = case args of
    "-normal-reduction" -> (state { reduction = ToNormal }, True)
    "-single-step-reduction" -> (state { reduction = SingleStep }, True)
    _ -> (state, False)
