import Data.Vect

data MyVect : Nat -> Type -> Type where
  Nil  : MyVect Z a
  (::) : (x : a) -> (xs: MyVect k a) -> MyVect (S k) a
  
%name MyVect xs, ys, zs  

append : MyVect n elem -> MyVect m elem -> MyVect (n + m) elem
append [] ys = ys
append (x :: xs) ys = x :: append xs ys

zip : MyVect n a -> MyVect n b -> MyVect n (a, b)
zip [] [] = []
zip (x :: xs) (y :: ys) = (x, y) :: zip xs ys

tryIndex : Integer -> Vect n a -> Maybe a
tryIndex {n} i xs = case integerToFin i n of
                         Nothing => Nothing
                         (Just idx) => Just (Data.Vect.index idx xs)

total vectTake : (n: Nat) -> Vect (n + m) a -> Vect n a
vectTake Z [] = []
vectTake Z (x :: xs) = []
vectTake (S k) (x :: xs) = x :: vectTake k xs

total sumEntries : Num a => (pos: Integer) -> Vect n a -> Vect n a -> Maybe a 
sumEntries pos [] [] = Nothing
sumEntries {n} pos xs ys = case integerToFin pos n of
                                          Nothing => Nothing
                                          (Just idx) => Just (index idx xs + index idx ys)


 
