
structure Cap_X : Type where
  Const ::
  f₁ : Nat
  f₂ : Nat
  f₃ : Nat
  f₄ : Nat
  f₅ : Nat

open Cap_X

def test : Cap_X := Const 1 1 1 1 1
