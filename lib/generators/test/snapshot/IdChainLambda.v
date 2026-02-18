
Module IdChainLambda.

Definition f {a : Type} : a -> a := fun x => x.

Definition test {a : Type} : a -> a := f f f f f f.

End IdChainLambda.
