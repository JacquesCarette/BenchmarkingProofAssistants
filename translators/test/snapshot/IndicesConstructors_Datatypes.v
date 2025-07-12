

Module IndicesConstructors_Datatypes.

Inductive D : nat -> nat -> nat -> nat -> nat -> Type :=
| C1 : forall {x1 : nat}, D X1 0 0 0 0
| C2 : forall {x1 x2 : nat}, D X1 X2 0 0 0
| C3 : forall {x1 x2 x3 : nat}, D X1 X2 X3 0 0
| C4 : forall {x1 x2 x3 x4 : nat}, D X1 X2 X3 X4 0
| C5 : forall {x1 x2 x3 x4 x5 : nat}, D X1 X2 X3 X4 X5.
End IndicesConstructors_Datatypes.