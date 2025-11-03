module NestedLetFunctions where

open import Agda.Builtin.Nat

n : Nat
n =
  let f₁ : (x₁ : Nat) → Nat
      f₁ x₁ = 1 + x₁
      f₂ : (x₁ : Nat) (x₂ : Nat) → Nat
      f₂ x₁ x₂ = 1 + x₁ + x₂
      f₃ : (x₁ : Nat) (x₂ : Nat) (x₃ : Nat) → Nat
      f₃ x₁ x₂ x₃ = 1 + x₁ + x₂ + x₃
      f₄ : (x₁ : Nat) (x₂ : Nat) (x₃ : Nat) (x₄ : Nat) → Nat
      f₄ x₁ x₂ x₃ x₄ = 1 + x₁ + x₂ + x₃ + x₄
      f₅ : (x₁ : Nat) (x₂ : Nat) (x₃ : Nat) (x₄ : Nat) (x₅ : Nat) → Nat
      f₅ x₁ x₂ x₃ x₄ x₅ = 1 + x₁ + x₂ + x₃ + x₄ + x₅
  in f₅ 2 3 4 5 6 + f₄ 2 3 4 5 + f₃ 2 3 4 + f₂ 2 3 + f₁ 2
