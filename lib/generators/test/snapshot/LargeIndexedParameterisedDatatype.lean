inductive D (p₁ : Type) (p₂ : Type) (p₃ : Type) (p₄ : Type) (p₅ : Type) : Nat → Nat → Nat → Nat → Nat → Type where
  | C : {x₁ x₂ x₃ x₄ x₅ : Nat} → D p₁ p₂ p₃ p₄ p₅ x₁ x₂ x₃ x₄ x₅

open D
