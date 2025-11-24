module ManyImplicits where

f : {a : Set} {x₀ x₁ x₂ x₃ x₄ x₅ : a} (y : a) → a
f {a = a} y = y
