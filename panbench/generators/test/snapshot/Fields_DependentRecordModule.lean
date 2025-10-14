
axiom P (n : Nat) : Type

axiom nil : P 0

axiom cons (n : Nat) (xs : P n) : P (Nat.succ n)

structure Cap_X : Type
  Const ::
  f₁ : Nat
  f₂ : P (Nat.succ f₁)
  f₃ : P (Nat.succ (Nat.succ f₁))
  f₄ : P (Nat.succ (Nat.succ (Nat.succ f₁)))
  f₅ : P (Nat.succ (Nat.succ (Nat.succ (Nat.succ f₁))))
  open Cap_X

def example : Cap_X :=
  Const
    (cons 0 nil)
    (cons 0 (cons 1 nil))
    (cons 0 (cons 1 (cons 2 nil)))
    (cons 0 (cons 1 (cons 2 (cons 3 nil))))
    (cons 0 (cons 1 (cons 2 (cons 3 (cons 4 nil)))))