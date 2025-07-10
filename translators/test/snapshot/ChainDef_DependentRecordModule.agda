module ChainDef_DependentRecordModule where
open import Agda.Builtin.Nat

record Dummy1 : Set where
    constructor Const1
    field
        f1 : Nat

record Dummy2 : Set where
    constructor Const2
    field
        f2 : Dummy1

record Dummy3 : Set where
    constructor Const3
    field
        f3 : Dummy2

record Dummy4 : Set where
    constructor Const4
    field
        f4 : Dummy3

record Dummy5 : Set where
    constructor Const5
    field
        f5 : Dummy4

example : Dummy5
example = Const5 (Const4 (Const3 (Const2 (Const1 10))))