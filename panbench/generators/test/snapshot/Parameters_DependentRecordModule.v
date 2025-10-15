
Module Parameters_DependentRecordModule.

Record X (f1 : nat) (f2 : nat) (f3 : nat) (f4 : nat) (f5 : nat) : Type := Const
  { sums : nat
  }.

Arguments Const {f1} {f2} {f3} {f4} {f5} _.

Definition test : X 1 2 3 4 5 := Const (1 + 2 + 3 + 4 + 5).

End Parameters_DependentRecordModule.