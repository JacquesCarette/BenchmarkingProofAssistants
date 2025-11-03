
Module NestedLetFunctions.

Definition n : nat :=
    let f1 (x1 : nat) : nat := 1 + x1
    in let f2 (x1 : nat) (x2 : nat) : nat := 1 + x1 + x2
    in let f3 (x1 : nat) (x2 : nat) (x3 : nat) : nat := 1 + x1 + x2 + x3
    in let f4 (x1 : nat) (x2 : nat) (x3 : nat) (x4 : nat) : nat :=
        1 + x1 + x2 + x3 + x4
    in let f5 (x1 : nat) (x2 : nat) (x3 : nat) (x4 : nat) (x5 : nat) : nat :=
        1 + x1 + x2 + x3 + x4 + x5
    in f5 2 3 4 5 6 + f4 2 3 4 5 + f3 2 3 4 + f2 2 3 + f1 2.

End NestedLetFunctions.