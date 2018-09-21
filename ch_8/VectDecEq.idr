data MyVect : Nat -> Type -> Type where
  Nil  : MyVect Z a
  (::) : (x : a) -> (xs: MyVect k a) -> MyVect (S k) a
  
%name MyVect xs, ys, zs  


headUnequal : DecEq a => {xs : MyVect n a} -> {ys : MyVect n a} -> (contra : x = y -> Void) -> ((x :: xs) = (y :: ys)) -> Void
headUnequal contra Refl = contra Refl

tailUnequal : DecEq a => {xs : MyVect n a} -> {ys : MyVect n a} -> (contra : xs = ys -> Void) -> ((x :: xs) = (y :: ys)) -> Void
tailUnequal contra Refl = contra Refl


decTailEquals : (DecEq a, DecEq (MyVect k a)) => (xs : MyVect k a) -> (ys : MyVect k a) -> Dec ((x :: xs) = (x :: ys))
decTailEquals xs ys = case decEq xs ys of
                       Yes prf   => Yes (cong prf)
                       No contra => No (tailUnequal contra)

DecEq a => DecEq (MyVect n a) where
    decEq [] [] = Yes Refl
    decEq (x :: xs) (y :: ys) = case decEq x y of
                                  Yes Refl => decTailEquals xs ys 
                                  No contra => No (headUnequal contra)
