
def n : Nat :=
  let f₁ (x₁ : Nat) : Nat := 1 + x₁
  let f₂ (x₁ : Nat) (x₂ : Nat) : Nat := 1 + x₁ + x₂
  let f₃ (x₁ : Nat) (x₂ : Nat) (x₃ : Nat) : Nat := 1 + x₁ + x₂ + x₃
  let f₄ (x₁ : Nat) (x₂ : Nat) (x₃ : Nat) (x₄ : Nat) : Nat :=
      1 + x₁ + x₂ + x₃ + x₄
  let f₅ (x₁ : Nat) (x₂ : Nat) (x₃ : Nat) (x₄ : Nat) (x₅ : Nat) : Nat :=
      1 + x₁ + x₂ + x₃ + x₄ + x₅
  f₅ 2 3 4 5 6 + f₄ 2 3 4 5 + f₃ 2 3 4 + f₂ 2 3 + f₁ 2