

Module DataImplicitIndices.

Inductive D : nat -> nat -> nat -> nat -> nat -> Type :=
| C1 : forall {x1 x2 x3 x4 x5 : nat}, D x1 x2 x3 x4 x5.
End DataImplicitIndices.