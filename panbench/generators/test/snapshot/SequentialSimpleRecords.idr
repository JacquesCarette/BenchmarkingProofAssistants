module Main

record Dummy1 where
  constructor Const1
  f1 : Nat

record Dummy2 where
  constructor Const2
  f2 : Nat

record Dummy3 where
  constructor Const3
  f3 : Nat

record Dummy4 where
  constructor Const4
  f4 : Nat

record Dummy5 where
  constructor Const5
  f5 : Nat

test : Dummy5
test = Const5 1

main : IO ()
main = putStrLn ""