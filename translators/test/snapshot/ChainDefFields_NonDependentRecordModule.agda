module ChainDefFields_NonDependentRecordModule where
open import Agda.Builtin.Nat

record Dummy1 : Set where
    constructor Const1
    field
        f1 : Nat

record Dummy2 : Set where
    constructor Const2
    field
        f2 : Nat

record Dummy3 : Set where
    constructor Const3
    field
        f3 : Nat

record Dummy4 : Set where
    constructor Const4
    field
        f4 : Nat

record Dummy5 : Set where
    constructor Const5
    field
        f5 : Nat

example : Dummy5
example = Const5 1