
def f {a : Type} : a → a := fun x ↦ x

def test {a : Type} : a → a := f f f f f f
