import Data.List.Views

equalSuffix : Eq a => List a -> List a -> List a
equalSuffix input1 input2 with (snocList input1)
  equalSuffix [] input2 | Empty = []
  equalSuffix (xs ++ [x]) input2 | (Snoc xsrec) with (snocList input2)
    equalSuffix (xs ++ [x]) [] | (Snoc xsrec) | Empty = []
    equalSuffix (xs ++ [x]) (ys ++ [y]) | (Snoc xsrec) | (Snoc ysrec) = 
      if x == y then equalSuffix xs ys | xsrec | ysrec ++ [x]
                else []


