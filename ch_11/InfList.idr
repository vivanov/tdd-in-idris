data InfList : Type -> Type where
  (::) : (value: elem) -> Inf (InfList elem) -> InfList elem
  
%name  InfList xs, ys, zs

countFrom : Integer -> InfList Integer
countFrom x = x :: Delay (countFrom(x + 1))

getPrefix : (count: Nat) -> InfList a -> List a
getPrefix Z xs = []
getPrefix (S k) (value :: xs) = value :: getPrefix k xs -- Delay/Force are added implicitly

Functor InfList where
  map func (x :: xs) = func x :: map func xs
