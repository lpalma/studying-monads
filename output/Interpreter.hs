module Interpreter where

type Environment = [(Name, Value)]
type Name = String
type O a = (String, a)

data Term = Var Name
          | Con Int
          | Add Term Term
          | Lam Name Term
          | App Term Term
          | Out Term

data Value = Wrong
           | Num Int
           | Fun (Value -> O Value)

unitO :: a -> O a
unitO a = ("", a)

bindO :: O a -> (a -> O b) -> O b
m `bindO` k = let (r, a) = m
                  (s, b) = k a
              in  (r ++ s, b)

showO :: O Value -> String
showO (s, a) = "Output: " ++ s ++ " Value: " ++ showval a

outO :: Value -> O ()
outO a = (showval a ++ "; ", ())

showval :: Value -> String
showval Wrong = "<wrong>"
showval (Num i) = show i
showval (Fun f) = "<function>"

interp :: Term -> Environment -> O Value
interp (Var x) e = fetch x e
interp (Con i) e = unitO (Num i)
interp (Add u v) e = interp u e `bindO` (\a ->
                     interp v e `bindO` (\b ->
                     add a b))
interp (Lam x v) e = unitO (Fun (\a -> interp v ((x,a):e)))
interp (App t u) e = interp t e `bindO` (\f ->
                     interp u e `bindO` (\a ->
                     apply f a))
interp (Out u) e = interp u e `bindO` (\a ->
                   outO a `bindO` (\() ->
                   unitO a))

fetch :: Name -> Environment -> O Value
fetch x [] = unitO Wrong
fetch x ((y,b):e) = if x==y then unitO b else fetch x e

add :: Value -> Value -> O Value
add (Num i) (Num j) = unitO (Num (i+j))
add a b = unitO Wrong

apply :: Value -> Value -> O Value
apply (Fun k) a = k a
apply f a = unitO Wrong
