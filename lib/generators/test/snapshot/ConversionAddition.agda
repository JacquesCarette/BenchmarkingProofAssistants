module ConversionAddition where

open import Agda.Builtin.Nat

open import Agda.Builtin.Equality

conv : 5 + 5 + 5 + 5 + 5 + 0 ≡ 25
conv = refl
