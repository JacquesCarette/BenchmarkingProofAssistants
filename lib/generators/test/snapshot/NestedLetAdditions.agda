module NestedLetAdditions where

open import Agda.Builtin.Nat

n : Nat
n =
  let x₁ = 1
      x₂ = x₁ + x₁
      x₃ = x₂ + x₂
      x₄ = x₃ + x₃
      x₅ = x₄ + x₄
  in x₅
