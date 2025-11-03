module Main

n : Nat
n =
  let x1 := 1
      x2 := x1 + x1
      x3 := x2 + x2
      x4 := x3 + x3
      x5 := x4 + x4
  in x5

main : IO ()
main = putStrLn ""