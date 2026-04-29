module LargeLambda where

const₅ : {A : Set} {B₀ B₁ B₂ B₃ B₄ : Set} → A → B₀ → B₁ → B₂ → B₃ → B₄ → A
const₅ = λ a b₀ b₁ b₂ b₃ b₄ → a
