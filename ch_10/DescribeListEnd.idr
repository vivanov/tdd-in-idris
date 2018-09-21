data ListLast : List a -> Type where
  Empty : ListLast []
  NonEmpty : (xs : List a) -> (x : a) -> ListLast (xs ++ [x])
  
describeHelper : (input : List Int) -> (form : ListLast input) -> String  
describeHelper [] Empty = "Empty"
describeHelper (xs ++ [x]) (NonEmpty xs x) = "Non-empty, initial portion = " ++ show xs

total
listLast : (xs : List a) -> ListLast xs
listLast [] = Empty
listLast (x :: xs) = case listLast xs of
                          Empty => NonEmpty [] x
                          NonEmpty ys y => NonEmpty (x :: ys) y
                          
describeListEnd : List Int -> String                          
describeListEnd xs = describeHelper xs (listLast xs)

describeListEnd2 : List Int -> String
describeListEnd2 input with (listLast input)
  describeListEnd2 [] | Empty = "Empty"
  describeListEnd2 (xs ++ [x]) | (NonEmpty xs x) = "Non-empty, initial portion = " ++ show xs
  
myReverse : List a -> List a
myReverse input with (listLast input)
  myReverse [] | Empty = []
  myReverse (xs ++ [x]) | (NonEmpty xs x) = x :: myReverse xs



