import data.Vect
reverse'_rhs_2 : (acc : Vect n1 a) -> Vect ((S n1) + k) a -> Vect (plus n1 (S k)) a
reverse'_rhs_2 {n1} {k} acc xs = rewrite sym (plusSuccRightSucc n1 k) in xs

myReverse : Vect n a -> Vect n a
myReverse xs = reverse' [] xs
  where reverse' : Vect n a -> Vect m a -> Vect (n + m) a
        reverse' {n} acc [] = rewrite plusZeroRightNeutral n in acc
        reverse' acc (x :: xs) = reverse'_rhs_2 acc (reverse' (x :: acc) xs) 
