module Interpreter where

type Environment = [(Name, Value)]
type Name = String
type L a = [a]

data Term = Var Name
          | Con Int
          | Add Term Term
          | Lam Name Term
          | App Term Term
          | Amb Term Term
          | Fail

data Value = Wrong
           | Num Int
           | Fun (Value ->  L Value)

unitL :: a -> L a
unitL a = [a]

bindL :: L a -> (a -> L b) -> L b
m `bindL` k = [ b | a <- m, b <- k a]

zeroL :: L a
zeroL = []

plusL :: L a -> L a -> L a
l `plusL` m = l ++ m

showL :: L Value -> String
showL m = show [ showval a | a <- m ]


showval :: Value -> String
showval Wrong = "<wrong>"
showval (Num i) = show i
showval (Fun f) = "<function>"

interp :: Term -> Environment ->  L Value
interp Fail e = zeroL
interp (Var x) e = fetch x e
interp (Con i) e = unitL (Num i)
interp (Add u v) e = interp u e `bindL` (\a ->
                     interp v e `bindL` (\b ->
                     add a b))
interp (Lam x v) e = unitL (Fun (\a -> interp v ((x,a):e)))
interp (App t u) e = interp t e `bindL` (\f ->
                     interp u e `bindL` (\a ->
                     apply f a))
interp (Amb u v) e = interp u e `plusL` interp v e

fetch :: Name -> Environment ->  L Value
fetch x [] = unitL Wrong
fetch x ((y,b):e) = if x==y then unitL b else fetch x e

add :: Value -> Value ->  L Value
add (Num i) (Num j) = unitL (Num (i+j))
add a b = unitL Wrong

apply :: Value -> Value ->  L Value
apply (Fun k) a = k a
apply f a = unitL Wrong
