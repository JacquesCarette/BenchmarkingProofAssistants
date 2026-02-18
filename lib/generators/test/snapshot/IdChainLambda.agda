module IdChainLambda where

f : {a : Set} → a → a
f = λ x → x

test : {a : Set} → a → a
test = f f f f f f
