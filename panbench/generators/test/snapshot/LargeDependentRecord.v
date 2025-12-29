
Module LargeDependentRecord.

Inductive P : forall (n : nat), Type :=
| PZ : P 0
| PS : forall {n : nat} (xs : P n), P (S n).

Record Cap_X : Type := Const
  { f1 : nat
  ; f2 : P (S f1)
  ; f3 : P (S (S f1))
  ; f4 : P (S (S (S f1)))
  ; f5 : P (S (S (S (S f1))))
  }.

Definition test : Cap_X :=
    Const 0 (PS PZ) (PS (PS PZ)) (PS (PS (PS PZ))) (PS (PS (PS (PS PZ)))).

End LargeDependentRecord.
