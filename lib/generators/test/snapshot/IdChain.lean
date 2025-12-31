
def id {A : Type} : A → A := fun x ↦ x

def test {A : Type} : A → A := id id id id id id
