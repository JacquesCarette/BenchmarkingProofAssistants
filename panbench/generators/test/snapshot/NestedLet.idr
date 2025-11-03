module Main

n : Nat
n =
  let x0 := 1
      x1 := x0
      x2 := x1
      x3 := x2
      x4 := x3
      x5 := x4
  in x5

main : IO ()
main = putStrLn ""