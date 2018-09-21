my_length: List a -> Nat
my_length [] = 0
my_length (x :: xs) = 1 + my_length xs
