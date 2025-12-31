module LargeLambda where

const₅ : {A : Set} {B₀ B₁ B₂ B₃ B₄ B₅ : Set} → A → B₀ → B₁ → B₂ → B₃ → B₄ → B₅ → A
const₅ = λ a b₀ b₁ b₂ b₃ b₄ b₅ → a
