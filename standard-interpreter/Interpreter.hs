module Interpreter where

type Environment = [(Name, Value)]
type I a = a
type Name = String

data Term = Var Name
          | Con Int
          | Add Term Term
          | Lam Name Term
          | App Term Term

data Value = Wrong
           | Num Int
           | Fun (Value -> I Value)

unitI :: a -> I a
unitI = id

bindI :: I a -> (a -> I b) -> I b
a `bindI` k = k a

showI :: I Value -> String
showI = showval

showval :: Value -> String
showval Wrong = "<wrong>"
showval (Num i) = show i
showval (Fun f) = "<function>"

interp :: Term -> Environment -> I Value
interp (Var x) e = fetch x e
interp (Con i) _ = Num i
interp (Add u v) e = add (interp u e) (interp v e)
interp (Lam x v) e = Fun (\a -> interp v ((x,a):e))
interp (App t u) e = apply (interp t e) (interp u e)

fetch :: Name -> Environment -> I Value
fetch _ [] = Wrong
fetch x ((y,b):e) = if x==y then b else fetch x e

add :: Value -> Value -> I Value
add (Num i) (Num j) = Num (i+j)
add _ _ = Wrong

apply :: Value -> Value -> I Value
apply (Fun k) a = k a
apply _ _ = Wrong

test :: Term -> String
test t = showI (interp t [])
