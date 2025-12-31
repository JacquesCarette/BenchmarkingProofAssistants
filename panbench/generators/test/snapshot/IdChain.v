
Module IdChain.

Definition id {A : Type} : A -> A := fun x => x.

Definition test {A : Type} : A -> A := id id id id id id.

End IdChain.
