import Lambda 

data StackAlphabet = Z | S | A
data State = Init | Body | AbstractionStart | AbstractionHead | AbstractionBody | SVar | SApp | Complete

parseToExpression :: String -> Expression
parseToExpression str = pda Init str [Z]

pda :: State -> String -> [StackAlphabet] -> Expression

pda Init (x:xs) [Z] = pda Body xs [s', Z]
    where
        s' = if x == '(' then A else S
pda Body (x:xs) stack = case x of
    '\\' -> pda AbstractionStart xs stack
    '('  -> pda Body xs (A:stack)
    _    -> pda SVar xs (S:stack)
    
pda AbstractionStart (_:xs) stack = pda AbstractionHead xs stack

pda AbstractionHead (x:xs) stack = case x of
    '.' -> pda AbstractionBody xs stack
    _   -> pda AbstractionHead xs stack

pda AbstractionBody (x:xs) stack = case x of
    '(' -> pda Body xs (A:stack)
    _   -> pda Body xs (S:stack)

pda SVar (x:xs) stack@(s:st) = case x of
    '(' -> pda Body xs (A:stack)
    ')' -> case s of 
        A -> pda SApp xs st
        _ -> error "mismatching brackets"
    _ -> pda SVar xs stack

pda SApp (x:xs) stack@(s:st) = case (x, s) of
    (_  , Z) -> pda Complete xs st
    (')', A) -> pda SApp xs st
    ('(', A) -> pda Body xs stack
    ('(', S) -> pda Body xs stack
    (_  , S) -> pda SApp xs st
    (_  , A) -> pda Body xs stack

-- TODO: need base case
pda Complete _ _ = Var "x"

pda _ _ _ = error "invalid state"

