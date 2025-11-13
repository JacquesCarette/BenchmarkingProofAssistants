
def id {A : Type} (x : A) : A := x

def test {A : Type} (x : A) : A := id id id id id x
