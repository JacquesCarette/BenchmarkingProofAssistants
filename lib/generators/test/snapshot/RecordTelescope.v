
Module RecordTelescope.

Record Telescope (U : Type) (El : U -> Type) : Type := Tele
  { a0 : U
  ; a1 : forall (x0 : El a0), U
  ; a2 : forall (x0 : El a0) (x1 : El (a1 x0)), U
  ; a3 : forall (x0 : El a0) (x1 : El (a1 x0)) (x2 : El (a2 x0 x1)), U
  ; a4 : forall (x0 : El a0) (x1 : El (a1 x0)) (x2 : El (a2 x0 x1)) (x3 : El
    (a3 x0 x1 x2)), U
  ; a5 : forall (x0 : El a0) (x1 : El (a1 x0)) (x2 : El (a2 x0 x1)) (x3 : El
    (a3 x0 x1 x2)) (x4 : El (a4 x0 x1 x2 x3)), U
  }.

Arguments Tele {U} {El} _ _ _ _ _ _.

End RecordTelescope.
