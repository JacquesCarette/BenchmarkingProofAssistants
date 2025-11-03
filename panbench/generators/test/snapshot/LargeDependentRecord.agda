module LargeDependentRecord where

open import Agda.Builtin.Nat

data P : (n : Nat) → Set where
  PZ : P 0
  PS : {n : Nat} (xs : P n) → P (suc n)

record Cap_X : Set where
  constructor Const
  field
    f₁ : Nat
    f₂ : P (suc f₁)
    f₃ : P (suc (suc f₁))
    f₄ : P (suc (suc (suc f₁)))
    f₅ : P (suc (suc (suc (suc f₁))))

open Cap_X

test : Cap_X
test = Const 0 (PS PZ) (PS (PS PZ)) (PS (PS (PS PZ))) (PS (PS (PS (PS PZ))))
