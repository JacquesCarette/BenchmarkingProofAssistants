
def id {A : Type} : A → A := (x : A) → x

def test {A : Type} : A → A := id id id id id id
