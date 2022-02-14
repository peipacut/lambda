module Lexer (parseToExpression) where

import Lambda 
import Data.Char
import Debug.Trace


data StackAlphabet = Z | A deriving (Show, Eq)
data State = Init | Body | AbstractionStart | AbstractionHead 
    | AbstractionBody | SVar | SApp | Complete 
        deriving Show

parseToExpression :: String -> Expression
parseToExpression str = pda Init str [Z]

-- see the visual representation of this automaton at docs/automaton.pdf
pda :: State -> String -> [StackAlphabet] -> Expression

pda Init (x:xs) [Z] 
    | x == '(' = trace' x "init " $ pda Body xs [A, Z]
    | otherwise = error "must start with ("

pda Body (x:xs) stack@(_:st)
    | x == '\\'  = trace' x "body" $ pda AbstractionStart xs stack
    | x == '('   = trace' x "body" $ pda Body xs (A:stack)
    | x == ')'   = trace' x "body" $ pda SApp xs st
    | isLetter x = trace' x "body" $ pda SVar xs stack
    | otherwise  = error "invalid token"
    
pda AbstractionStart (x:xs) stack  
    | isLetter x = trace' x "abstractionstart" $ pda AbstractionHead xs stack
    | isSpace x  = trace' x "abstractionstart" $ pda AbstractionStart xs stack
    | otherwise  = error "bound variable must start with a letter"

pda AbstractionHead (x:xs) stack 
    | isAlphaNum x = trace' x "abstractionhead" $ pda AbstractionHead xs stack
    | x == '.'     = trace' x "abstractionhead" $ pda AbstractionBody xs stack
    | otherwise    = error "bound variable must have one letter followed by alphanumeric characters"
    
pda AbstractionBody (x:xs) stack = case x of
    '(' -> trace' x "abstractionbody" $ pda Body xs (A:stack)
    _   -> error "must start new scope with ( after variable bind"

pda SVar (x:xs) stack@(_:st) 
    | x == '('     = trace' x "svar" $ pda Body xs (A:stack)
    | x == ')'     = trace' x "svar" $ pda SApp xs (st)
    | isAlphaNum x = trace' x "svar" $ pda SVar xs stack

pda SApp s@(x:xs) stack@(_:st) 
    | x == ')'               = trace' x "sapp" $ pda SApp xs st
    | x == '(' || isLetter x = trace' x "sapp" $ pda Body xs stack

pda SApp [] [Z] = pda Complete [] [Z]

pda Complete _ _ = Var "yatta"

pda a b c = error ("invalid state -> " ++ show a ++ " " ++ show b ++ " " ++ show c)

showTrace = True

trace' x s f 
    | showTrace = trace (s ++ " " ++ show x) f
    | otherwise = f
