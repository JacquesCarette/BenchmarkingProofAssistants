Require Import Coq.Vectors.Vector. 
Require Import List. 
Import VectorNotations. 
Import ListNotations.


Module LetExample.


Definition n : nat := let x1 := 1 in
    let x2 := x1 in
    let x3 := x2 in
    let x4 := x3 in
    let x5 := x4 in
    x5. 

End LetExample.