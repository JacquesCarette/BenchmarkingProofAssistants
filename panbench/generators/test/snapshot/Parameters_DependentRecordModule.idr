module Main

record X (f1 : Nat) (f2 : Nat) (f3 : Nat) (f4 : Nat) (f5 : Nat) where
  constructor Const
  sums : Nat

test : X 1 2 3 4 5
test = Const (1 + 2 + 3 + 4 + 5)

main : IO ()
main = putStrLn ""