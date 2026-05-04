
Module NestedLetXor.

Definition b : bool :=
    let x1 := true
    in let x2 := xorb x1 x1
    in let x3 := xorb x2 x2
    in let x4 := xorb x3 x3
    in let x5 := xorb x4 x4
    in x5.

End NestedLetXor.
