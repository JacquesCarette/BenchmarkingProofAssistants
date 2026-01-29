module Main

f : {a : Type} -> a -> a
f = \x => x

test : {a : Type} -> a -> a
test = f f f f f f

main : IO ()
main = putStrLn ""
