module Interpreter where

type Environment = [(Name, Value)]
type Name = String
type S a = State -> (a, State)
type State = Int

data Term = Var Name
          | Con Int
          | Add Term Term
          | Lam Name Term
          | App Term Term
          | Count

data Value = Wrong
           | Num Int
           | Fun (Value -> S Value)

unitS :: a -> S a
unitS a = \s0 -> (a, s0)

bindS :: S a -> (a -> S b) -> S b
m `bindS` k = \s2 -> let (a, s0) = m s1
                         (b, s1) = k a s2
                     in  (b, s0)

showS :: S Value -> String
showS m = let (a, s1) = m 0
          in  "Value: " ++ showval a ++ "; " ++
              "Count: " ++ show s1

tickS :: S ()
tickS = \s -> ((), s + 1)

fetchS :: S State
fetchS = \s -> (s, s)

showval :: Value -> String
showval Wrong = "<wrong>"
showval (Num i) = show i
showval (Fun f) = "<function>"

interp :: Term -> Environment -> S Value
interp Count e = fetchS `bindS` (\i -> unitS (Num i))
interp (Var x) e = fetch x e
interp (Con i) e = unitS (Num i)
interp (Add u v) e = interp u e `bindS` (\a ->
                     interp v e `bindS` (\b ->
                     add a b))
interp (Lam x v) e = unitS (Fun (\a -> interp v ((x,a):e)))
interp (App t u) e = interp t e `bindS` (\f ->
                     interp u e `bindS` (\a ->
                     apply f a))

fetch :: Name -> Environment -> S Value
fetch x [] = unitS Wrong
fetch x ((y,b):e) = if x==y then unitS b else fetch x e

add :: Value -> Value -> S Value
add (Num i) (Num j) = tickS `bindS` (\() -> unitS (Num (i+j)))
add a b = unitS Wrong

apply :: Value -> Value -> S Value
apply (Fun k) a = tickS `bindS` (\() -> k a)
apply f a = unitS Wrong

test :: Term -> String
test t = showS (interp t [])
