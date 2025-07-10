module Pattern_Matching_Datatypes where
open import Agda.Builtin.Nat

data D : Set where
 C1 : D
 C2 : D
 C3 : D
 C4 : D
 C5 : D


F : (C : D) -> Nat
F C1 = 1
F C2 = 2
F C3 = 3
F C4 = 4
F C5 = 5
N : Nat
N = F C5 + F C4 + F C3 + F C2 + F C1
