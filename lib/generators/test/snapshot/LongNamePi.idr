module Main

f : {a : Type} -> Type
f {a = a} = (x, yyyyy : a) -> a

main : IO ()
main = putStrLn ""
