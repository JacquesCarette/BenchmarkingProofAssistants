
Module IdChain.

Definition f {a : Type} (x : a) : a := x.

Definition test {a : Type} : a -> a := f f f f f f.

End IdChain.
