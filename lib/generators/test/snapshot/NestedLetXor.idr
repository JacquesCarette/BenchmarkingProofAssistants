module Main

import Data.Bool.Xor

b : Bool
b =
  let x1 := True
      x2 := xor x1 x1
      x3 := xor x2 x2
      x4 := xor x3 x3
      x5 := xor x4 x4
  in x5

main : IO ()
main = putStrLn ""
