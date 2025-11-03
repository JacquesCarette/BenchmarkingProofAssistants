module Main

data D : (p1 : Type) ->
         (p2 : Type) ->
         (p3 : Type) ->
         (p4 : Type) ->
         (p5 : Type) ->
         Nat -> Nat -> Nat -> Nat -> Nat -> Type
  where
  C : (p1 : Type) ->
      (p2 : Type) ->
      (p3 : Type) ->
      (p4 : Type) ->
      (p5 : Type) ->
      {x1, x2, x3, x4, x5 : Nat} -> D p1 p2 p3 p4 p5 x1 x2 x3 x4 x5

main : IO ()
main = putStrLn ""