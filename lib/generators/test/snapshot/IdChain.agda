module IdChain where

id : {A : Set} (x : A) → A
id x = x

test : {A : Set} (x : A) → A
test x = id id id id id id x
