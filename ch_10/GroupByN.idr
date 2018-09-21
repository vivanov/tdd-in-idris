data TakeN : List a -> Type where
  Fewer : TakeN xs
  Exact : (n_xs : List a) -> TakeN (n_xs ++ rest)

takeN : (n : Nat) -> (xs : List a) -> TakeN xs
takeN _ [] = Fewer
takeN Z _ = Exact []
takeN (S k) (y :: ys) = case takeN k ys of
                          Fewer => Fewer
                          Exact xs => Exact (y :: xs)

groupByN : (n : Nat) -> (xs : List a) -> List (List a)
groupByN n xs with (takeN n xs)
  groupByN n xs | Fewer = [xs]
  groupByN n (n_xs ++ rest) | (Exact n_xs) = n_xs :: groupByN n rest


{--
data Length : (List a, List a) -> Type where
  LessThanOne : Length ([] , xs)
  MoreThanOne : (xs1 : List a) -> (xs2 : List a) -> Length (xs1, xs2)
  
length : (n : Nat) -> (xs : List a) -> Length xs
--}
  

halves : List a -> (List a, List a)
halves xs with (takeN ((length xs) `div` 2)  xs)
  halves xs | Fewer = ([], xs)
  halves (n_xs ++ rest) | (Exact n_xs) = (n_xs, rest)

