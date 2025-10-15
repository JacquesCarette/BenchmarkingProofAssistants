
Module Fields_NonDependentRecordModule.

Record Cap_X : Type := Const
  { f1 : nat
  ; f2 : nat
  ; f3 : nat
  ; f4 : nat
  ; f5 : nat
  }.

Definition test : Cap_X := Const 1 1 1 1 1.

End Fields_NonDependentRecordModule.