module Parameters_DependentRecordModule where

open import Agda.Builtin.Nat

record X (f₁ : Nat) (f₂ : Nat) (f₃ : Nat) (f₄ : Nat) (f₅ : Nat) : Set where
  constructor Const
  field
    sums : Nat

open X

test : X 1 2 3 4 5
test = Const (1 + 2 + 3 + 4 + 5)
