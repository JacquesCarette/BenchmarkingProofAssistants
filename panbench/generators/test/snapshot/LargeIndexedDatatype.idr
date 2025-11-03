module Main

data D : Nat -> Nat -> Nat -> Nat -> Nat -> Type where
  C1 : {x1 : Nat} -> D x1 0 0 0 0
  C2 : {x1 : Nat} -> {x2 : Nat} -> D x1 x2 0 0 0
  C3 : {x1 : Nat} -> {x2 : Nat} -> {x3 : Nat} -> D x1 x2 x3 0 0
  C4 : {x1 : Nat} -> {x2 : Nat} -> {x3 : Nat} -> {x4 : Nat} -> D x1 x2 x3 x4 0
  C5 : {x1 : Nat} ->
       {x2 : Nat} ->
       {x3 : Nat} ->
       {x4 : Nat} ->
       {x5 : Nat} ->
       D x1 x2 x3 x4 x5

main : IO ()
main = putStrLn ""