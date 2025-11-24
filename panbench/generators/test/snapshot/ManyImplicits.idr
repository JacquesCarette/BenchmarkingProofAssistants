module Main

f : {a : Type} -> {x0, x1, x2, x3, x4, x5 : a} -> (y : a) -> a
f {a = a} y = y

main : IO ()
main = putStrLn ""
