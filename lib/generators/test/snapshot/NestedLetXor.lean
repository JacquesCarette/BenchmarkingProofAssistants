def b : Bool :=
  let x₁ := Bool.true
  let x₂ := x₁ ^^ x₁
  let x₃ := x₂ ^^ x₂
  let x₄ := x₃ ^^ x₃
  let x₅ := x₄ ^^ x₄
  x₅
