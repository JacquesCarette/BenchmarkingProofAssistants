
Module IdChain.

Definition id {A : Type} : A -> A := forall (x : A), x.

Definition test {A : Type} : A -> A := id id id id id id.

End IdChain.
