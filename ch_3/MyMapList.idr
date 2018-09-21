my_map : (a -> b) -> List a -> List b 
my_map _ [] = []
my_map f (x :: xs) = f x :: my_map f xs

