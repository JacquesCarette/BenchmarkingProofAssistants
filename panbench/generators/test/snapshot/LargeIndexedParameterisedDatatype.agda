module LargeIndexedParameterisedDatatype where

open import Agda.Builtin.Nat

data D (p₁ : Set) (p₂ : Set) (p₃ : Set) (p₄ : Set) (p₅ : Set) : Nat → Nat → Nat → Nat → Nat → Set where
  C : {x₁ x₂ x₃ x₄ x₅ : Nat} → D p₁ p₂ p₃ p₄ p₅ x₁ x₂ x₃ x₄ x₅
