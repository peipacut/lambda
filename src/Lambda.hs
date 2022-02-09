module Lambda 
( alphaConvert
, betaReduce
, Variable 
, Expression (..)
) where


type Variable = Char

data Expression = Var Variable | Abstract Variable Expression | Apply Expression Expression deriving Eq

instance Show Expression where
    show (Var v) = show v
    show (Abstract var term) = "\\" ++ (show var) ++ "." ++ (show term)
    show (Apply t1 t2) = "(" ++ (show t1) ++ ")(" ++ (show t2) ++ ")"

-- | applies alpha conversion on the given term by replacing the bound
--   variable with the given one. Only works on abstractions.
alphaConvert :: Expression -> Variable -> Expression
alphaConvert (Abstract var term) replace = Abstract replace (convert term)
    where
        convert :: Expression -> Expression
        convert (Var x) = Var (if x == var then replace else x)
        convert (Apply t1 t2) = Apply (convert t1) (convert t2)
        convert apply = apply
alphaConvert _ _ 
    = error ("can only apply alpha conversion on abstractions, " 
        ++ "ie lambda terms of the form (\\x.M[x])")

-- | applies a lazy beta reduction using deterministic sematics to a term once
betaReduce :: Expression -> Expression
betaReduce (Apply (Abstract x m) e) = substitute m e x
betaReduce e = e

-- this probably works
-- | analogous to exp[m\x], m -> x in x, (replace all x with m in exp, m into x) 
--   main expression -> expression to replace with -> variable to replace -> output expression
substitute :: Expression -> Expression -> Variable -> Expression
substitute t@(Var v) m x 
    | v == x    = m
    | otherwise = t
substitute t@(Abstract b e) m x 
    -- x is already bound, nothing to do here..
    | b == x    = t 
    -- x is unbound
    | otherwise = Abstract b (substitute e m x)
substitute (Apply e1 e2) m x = Apply (substitute e1 m x) (substitute e2 m x)
