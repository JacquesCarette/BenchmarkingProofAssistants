
inductive P : (n : Nat) → Type where
  | PZ : P 0
  | PS : {n : Nat} → (xs : P n) → P (Nat.succ n)

open P

structure Cap_X : Type where
  Const ::
  f₁ : Nat
  f₂ : P (Nat.succ f₁)
  f₃ : P (Nat.succ (Nat.succ f₁))
  f₄ : P (Nat.succ (Nat.succ (Nat.succ f₁)))
  f₅ : P (Nat.succ (Nat.succ (Nat.succ (Nat.succ f₁))))

open Cap_X

def test : Cap_X :=
  Const 0 (PS PZ) (PS (PS PZ)) (PS (PS (PS PZ))) (PS (PS (PS (PS PZ))))
