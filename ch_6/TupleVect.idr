TypleVect : (num : Nat) -> Type -> Type
TypleVect Z _ = ()
TypleVect (S k) t = (t, TypleVect k t)

test: TypleVect 4 Nat
test = (1, 2, 3, 4, ())

