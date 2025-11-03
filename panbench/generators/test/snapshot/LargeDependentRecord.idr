module Main

data P : (n : Nat) -> Type where
  PZ : P 0
  PS : {n : Nat} -> (xs : P n) -> P (S n)

record Cap_X where
  constructor Const
  f1 : Nat
  f2 : P (S f1)
  f3 : P (S (S f1))
  f4 : P (S (S (S f1)))
  f5 : P (S (S (S (S f1))))

test : Cap_X
test = Const 0 (PS PZ) (PS (PS PZ)) (PS (PS (PS PZ))) (PS (PS (PS (PS PZ))))

main : IO ()
main = putStrLn ""