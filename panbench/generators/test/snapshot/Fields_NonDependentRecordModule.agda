module Fields_NonDependentRecordModule where

open import Agda.Builtin.Nat

record Cap_X : Set where
  constructor Const
  field
    f₁ : Nat
    f₂ : Nat
    f₃ : Nat
    f₄ : Nat
    f₅ : Nat

open Cap_X

test : Cap_X
test = Const 1 1 1 1 1
