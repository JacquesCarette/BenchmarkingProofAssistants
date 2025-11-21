module Main

f : {A : Type} -> {x0, x1, x2, x3, x4, x5 : A} -> (y : A) -> A
f A x0 x1 x2 x3 x4 x5 y = y

main : IO ()
main = putStrLn ""