
structure Dummy₁ : Type where
  Const₁ ::
  f₁ : Nat

open Dummy₁

structure Dummy₂ : Type where
  Const₂ ::
  f₂ : Nat

open Dummy₂

structure Dummy₃ : Type where
  Const₃ ::
  f₃ : Nat

open Dummy₃

structure Dummy₄ : Type where
  Const₄ ::
  f₄ : Nat

open Dummy₄

structure Dummy₅ : Type where
  Const₅ ::
  f₅ : Nat

open Dummy₅

def test : Dummy₅ := Const₅ 1