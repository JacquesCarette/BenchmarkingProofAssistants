
Module LargeIndexedDatatype.

Inductive D : nat -> nat -> nat -> nat -> nat -> Type :=
| C1 : forall {x1 : nat}, D x1 0 0 0 0
| C2 : forall {x1 : nat} {x2 : nat}, D x1 x2 0 0 0
| C3 : forall {x1 : nat} {x2 : nat} {x3 : nat}, D x1 x2 x3 0 0
| C4 : forall {x1 : nat} {x2 : nat} {x3 : nat} {x4 : nat}, D x1 x2 x3 x4 0
| C5 :
    forall {x1 : nat} {x2 : nat} {x3 : nat} {x4 : nat} {x5 : nat},
    D x1 x2 x3 x4 x5.

End LargeIndexedDatatype.
