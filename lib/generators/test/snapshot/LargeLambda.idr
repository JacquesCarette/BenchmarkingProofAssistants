module Main

const5 : {A : Type} ->
         {B0, B1, B2, B3, B4, B5 : Type} ->
         A -> B0 -> B1 -> B2 -> B3 -> B4 -> B5 -> A
const5 = \a, b0, b1, b2, b3, b4, b5 => a

main : IO ()
main = putStrLn ""
