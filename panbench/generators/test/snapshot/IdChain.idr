module Main

id : {A : Type} -> (x : A) -> A
id x = x

test : {A : Type} -> (x : A) -> A
test x = id id id id id x

main : IO ()
main = putStrLn ""
