

Module Pattern_Matching_Datatypes.

Inductive D : Type :=
| C1 : D
| C2 : D
| C3 : D
| C4 : D
| C5 : D.

Fixpoint F (C : D) : nat :=
match C with
| C1 => 1
| C2 => 2
| C3 => 3
| C4 => 4
| C5 => 5 end.

Definition N : nat := F C5 + F C4 + F C3 + F C2 + F C1.

End Pattern_Matching_Datatypes.