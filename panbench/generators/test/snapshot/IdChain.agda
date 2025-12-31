module IdChain where

id : {A : Set} → A → A
id {A = A} = λ x → x

test : {A : Set} → A → A
test {A = A} = id id id id id id
