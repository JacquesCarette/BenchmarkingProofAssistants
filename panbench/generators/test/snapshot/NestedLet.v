
Module NestedLet.

Definition n : nat :=
    let x0 := 1
    in let x1 := x0
    in let x2 := x1
    in let x3 := x2
    in let x4 := x3
    in let x5 := x4
    in x5.

End NestedLet.
