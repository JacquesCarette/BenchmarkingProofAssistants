
Module IdChain.

Definition id {A : Type} (x : A) : A := x.

Definition test {A : Type} (x : A) : A := id id id id id id x.

End IdChain.
