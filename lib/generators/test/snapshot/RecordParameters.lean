structure X (f₁ : Nat) (f₂ : Nat) (f₃ : Nat) (f₄ : Nat) (f₅ : Nat) : Type where
  Const ::
  sums : Nat

open X

def test : X 1 2 3 4 5 := Const (1 + 2 + 3 + 4 + 5)
