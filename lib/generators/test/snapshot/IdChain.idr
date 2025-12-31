module Main

id : {A : Type} -> A -> A
id {A = A} = (x : A) -> x

test : {A : Type} -> A -> A
test {A = A} = id id id id id id

main : IO ()
main = putStrLn ""
