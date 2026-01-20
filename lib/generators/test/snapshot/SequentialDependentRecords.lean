
structure Dummy₁ : Type where
  Const₁ ::
  f₁ : Nat

open Dummy₁

structure Dummy₂ : Type where
  Const₂ ::
  f₂ : Dummy₁

open Dummy₂

structure Dummy₃ : Type where
  Const₃ ::
  f₃ : Dummy₂

open Dummy₃

structure Dummy₄ : Type where
  Const₄ ::
  f₄ : Dummy₃

open Dummy₄

structure Dummy₅ : Type where
  Const₅ ::
  f₅ : Dummy₄

open Dummy₅

def test : Dummy₅ := (Const₅ (Const₄ (Const₃ (Const₂ (Const₁ 10)))))
