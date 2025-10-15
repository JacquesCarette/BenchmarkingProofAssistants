module ChainDefFields_NonDependentRecordModule where

open import Agda.Builtin.Nat

record Dummy₁ : Set where
  constructor Const₁
  field
    f₁ : Nat

open Dummy₁

record Dummy₂ : Set where
  constructor Const₂
  field
    f₂ : Nat

open Dummy₂

record Dummy₃ : Set where
  constructor Const₃
  field
    f₃ : Nat

open Dummy₃

record Dummy₄ : Set where
  constructor Const₄
  field
    f₄ : Nat

open Dummy₄

record Dummy₅ : Set where
  constructor Const₅
  field
    f₅ : Nat

open Dummy₅

test : Dummy₅
test = Const₅ 1
