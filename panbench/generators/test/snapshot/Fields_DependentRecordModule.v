
Module Fields_DependentRecordModule.

Axiom P : forall (n : nat), Type.

Axiom nil : P 0.

Axiom cons : forall (n : nat) (xs : P n), P (S n).

Record Cap_X : Type := Const
  { f1 : nat
  ; f2 : P (S f1)
  ; f3 : P (S (S f1))
  ; f4 : P (S (S (S f1)))
  ; f5 : P (S (S (S (S f1))))
  }.

Definition example : Cap_X :=
    Const
      (cons 0 nil)
      (cons 0 (cons 1 nil))
      (cons 0 (cons 1 (cons 2 nil)))
      (cons 0 (cons 1 (cons 2 (cons 3 nil))))
      (cons 0 (cons 1 (cons 2 (cons 3 (cons 4 nil))))).

End Fields_DependentRecordModule.