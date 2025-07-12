
structure Dummy1 where
    Const1 ::
    f1 : Nat

structure Dummy2 where
    Const2 ::
    f2 : Nat

structure Dummy3 where
    Const3 ::
    f3 : Nat

structure Dummy4 where
    Const4 ::
    f4 : Nat

structure Dummy5 where
    Const5 ::
    f5 : Nat

open Dummy1 Dummy2 Dummy3 Dummy4 Dummy5
example : Dummy5 := Const5 1