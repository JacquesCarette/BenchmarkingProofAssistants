
inductive D : Nat → Nat → Nat → Nat → Nat → Type where
  | C₁ : {x₁ : Nat} → D x₁ 0 0 0 0
  | C₂ : {x₁ : Nat} → {x₂ : Nat} → D x₁ x₂ 0 0 0
  | C₃ : {x₁ : Nat} → {x₂ : Nat} → {x₃ : Nat} → D x₁ x₂ x₃ 0 0
  | C₄ : {x₁ : Nat} → {x₂ : Nat} → {x₃ : Nat} → {x₄ : Nat} → D x₁ x₂ x₃ x₄ 0
  | C₅ : {x₁ : Nat} →
         {x₂ : Nat} →
         {x₃ : Nat} →
         {x₄ : Nat} →
         {x₅ : Nat} →
         D x₁ x₂ x₃ x₄ x₅

open D