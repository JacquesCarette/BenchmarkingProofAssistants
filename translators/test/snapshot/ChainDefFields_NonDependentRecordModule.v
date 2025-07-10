

Module ChainDefFields_NonDependentRecordModule.

Record Dummy1 : Type := Const1 {
  f1 : nat;
}.

Record Dummy2 : Type := Const2 {
  f2 : nat;
}.

Record Dummy3 : Type := Const3 {
  f3 : nat;
}.

Record Dummy4 : Type := Const4 {
  f4 : nat;
}.

Record Dummy5 : Type := Const5 {
  f5 : nat;
}.

Definition example : Dummy5 :=
  Const5 1.

End ChainDefFields_NonDependentRecordModule.