
inductive D : Type where
| C1 : D
| C2 : D
| C3 : D
| C4 : D
| C5 : D
open D
def F : (C : D) -> Nat
| C1 => 1
| C2 => 2
| C3 => 3
| C4 => 4
| C5 => 5
def N : Nat := F C5 + F C4 + F C3 + F C2 + F C1