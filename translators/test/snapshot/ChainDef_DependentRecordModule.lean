
structure Dummy1 where
    Const1 ::
    f1 : Nat

structure Dummy2 where
    Const2 ::
    f2 : Dummy1

structure Dummy3 where
    Const3 ::
    f3 : Dummy2

structure Dummy4 where
    Const4 ::
    f4 : Dummy3

structure Dummy5 where
    Const5 ::
    f5 : Dummy4

open Dummy1 Dummy2 Dummy3 Dummy4 Dummy5
example : Dummy5 := Const5 (Const4 (Const3 (Const2 (Const1 10))))