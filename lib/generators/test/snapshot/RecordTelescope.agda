module RecordTelescope where

record Telescope (U : Set) (El : U → Set) : Set where
  constructor Tele
  field
    a₀ : U
    a₁ : (x₀ : El a₀) → U
    a₂ : (x₀ : El a₀) (x₁ : El (a₁ x₀)) → U
    a₃ : (x₀ : El a₀) (x₁ : El (a₁ x₀)) (x₂ : El (a₂ x₀ x₁)) → U
    a₄ :
      (x₀ : El a₀) (x₁ : El (a₁ x₀)) (x₂ : El (a₂ x₀ x₁)) (x₃ : El
        (a₃ x₀ x₁ x₂)) → U
    a₅ :
      (x₀ : El a₀) (x₁ : El (a₁ x₀)) (x₂ : El (a₂ x₀ x₁)) (x₃ : El
        (a₃ x₀ x₁ x₂)) (x₄ : El (a₄ x₀ x₁ x₂ x₃)) → U

open Telescope
