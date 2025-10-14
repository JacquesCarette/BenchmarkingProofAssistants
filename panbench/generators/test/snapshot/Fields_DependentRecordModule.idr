module Main

P : (n : Nat) -> Type
P n = believe_me ()

nil : P 0
nil = believe_me ()

cons : (n : Nat) -> (xs : P n) -> P (S n)
cons n xs = believe_me ()

record Cap_X where
  constructor Const
  f1 : Nat
  f2 : P (S f1)
  f3 : P (S (S f1))
  f4 : P (S (S (S f1)))
  f5 : P (S (S (S (S f1))))

example : Cap_X
example =
  Const
    (cons 0 nil)
    (cons 0 (cons 1 nil))
    (cons 0 (cons 1 (cons 2 nil)))
    (cons 0 (cons 1 (cons 2 (cons 3 nil))))
    (cons 0 (cons 1 (cons 2 (cons 3 (cons 4 nil)))))

main : IO ()
main = putStrLn ""