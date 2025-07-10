

Module IndicesParameters_Datatypes.

Inductive D (p1 : Type) (p2 : Type) (p3 : Type) (p4 : Type) (p5 : Type) : nat -> nat -> nat -> nat -> nat -> Type :=
| C : forall {X1 X2 X3 X4 X5 : nat}, D p1 p2 p3 p4 p5 X1 X2 X3 X4 X5.
End IndicesParameters_Datatypes.