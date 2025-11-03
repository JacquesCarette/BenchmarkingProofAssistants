module Main

data D : (p1 : Type) ->
         (p2 : Type) ->
         (p3 : Type) ->
         (p4 : Type) ->
         (p5 : Type) ->
         Type
  where
  C : (p1 : Type) ->
      (p2 : Type) ->
      (p3 : Type) ->
      (p4 : Type) ->
      (p5 : Type) ->
      D p1 p2 p3 p4 p5

main : IO ()
main = putStrLn ""