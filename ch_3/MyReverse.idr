my_reverse: List a -> List a
my_reverse xs = go [] xs
                where 
                 go: List a -> List a -> List a
                 go acc [] = acc
                 go acc (y :: ys) = go (y :: acc) ys
                                                          

