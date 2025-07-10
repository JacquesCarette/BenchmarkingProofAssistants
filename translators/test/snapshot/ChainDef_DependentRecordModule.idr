module Main

record Dummy1 where
    constructor Const1
    f1 : Nat

record Dummy2 where
    constructor Const2
    f2 : Dummy1

record Dummy3 where
    constructor Const3
    f3 : Dummy2

record Dummy4 where
    constructor Const4
    f4 : Dummy3

record Dummy5 where
    constructor Const5
    f5 : Dummy4

example : Dummy5
example = Const5 (Const4 (Const3 (Const2 (Const1 10))))

main : IO()
main = putStrLn ""