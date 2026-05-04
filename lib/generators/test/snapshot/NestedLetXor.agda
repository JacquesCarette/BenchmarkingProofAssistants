module NestedLetXor where

open import Agda.Builtin.Bool

xor : Bool → Bool → Bool
xor true true = false
xor false false = false
xor _ _ = true

b : Bool
b =
  let x₁ = true
      x₂ = xor x₁ x₁
      x₃ = xor x₂ x₂
      x₄ = xor x₃ x₃
      x₅ = xor x₄ x₄
  in x₅
