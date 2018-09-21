data MyVect : Nat -> Type -> Type where
  Nil  : MyVect Z a
  (::) : (x : a) -> (xs: MyVect k a) -> MyVect (S k) a
  
%name MyVect xs, ys, zs  

Eq t => Eq (MyVect n t) where
    (==) [] [] = True
    (==) (x :: xs) (y :: ys) = (x == y) && (xs == ys)
    (/=) xs ys = not (xs == ys)

Foldable (MyVect n) where
    foldr func init [] = init
    foldr func init (x :: xs) = func x (foldr func init xs)
    foldl func init [] = init
    foldl func init (x :: xs) = foldl func (func init x) xs


 
