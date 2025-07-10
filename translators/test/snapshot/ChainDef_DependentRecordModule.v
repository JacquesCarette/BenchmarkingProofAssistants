

Module ChainDef_DependentRecordModule.

Record Dummy1 : Type := Const1 {
  f1 : nat;
}.

Record Dummy2 : Type := Const2 {
  f2 : Dummy1;
}.

Record Dummy3 : Type := Const3 {
  f3 : Dummy2;
}.

Record Dummy4 : Type := Const4 {
  f4 : Dummy3;
}.

Record Dummy5 : Type := Const5 {
  f5 : Dummy4;
}.

Definition example : Dummy5 :=
  Const5 (Const4 (Const3 (Const2 (Const1 10)))).

End ChainDef_DependentRecordModule.