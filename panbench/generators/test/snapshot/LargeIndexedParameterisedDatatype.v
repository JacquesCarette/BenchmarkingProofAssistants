
Module LargeIndexedParameterisedDatatype.

Inductive D (p1 : Type) (p2 : Type) (p3 : Type) (p4 : Type) (p5 : Type) : nat -> nat -> nat -> nat -> nat -> Type :=
| C : forall {x1 x2 x3 x4 x5 : nat}, D p1 p2 p3 p4 p5 x1 x2 x3 x4 x5.

End LargeIndexedParameterisedDatatype.