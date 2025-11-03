module NestedLet where

open import Agda.Builtin.Nat

n : Nat
n =
  let x₀ = 1
      x₁ = x₀
      x₂ = x₁
      x₃ = x₂
      x₄ = x₃
      x₅ = x₄
  in x₅
