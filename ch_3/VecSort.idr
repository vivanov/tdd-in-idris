import Data.Vect

total insert : Ord elem => (x : elem) -> (xs_sorted : Vect len elem) -> Vect (S len) elem
insert x [] = [x]
insert x (y :: xs) = case x < y of
                          False => y :: insert x xs
                          True => x :: y :: xs

total insSort : Ord elem => Vect n elem -> Vect n elem
insSort [] = []
insSort (x :: xs) = let xs_sorted = insSort xs in
                        insert x xs_sorted

