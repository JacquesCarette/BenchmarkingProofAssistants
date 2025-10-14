module Fields_DependentRecordModule where

postulate
  P : (n : Nat) → Set

postulate
  nil : P 0

postulate
  cons : (n : Nat) (xs : P n) → P (suc n)

record Cap_X : Set where
  constructor Const
  fields
    f₁ : Nat
    f₂ : P (suc f₁)
    f₃ : P (suc (suc f₁))
    f₄ : P (suc (suc (suc f₁)))
    f₅ : P (suc (suc (suc (suc f₁))))

example : Cap_X
example =
  Const
    (cons 0 nil)
    (cons 0 (cons 1 nil))
    (cons 0 (cons 1 (cons 2 nil)))
    (cons 0 (cons 1 (cons 2 (cons 3 nil))))
    (cons 0 (cons 1 (cons 2 (cons 3 (cons 4 nil)))))
