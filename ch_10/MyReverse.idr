{-
data SnocList : (ty : Type) -> Type where
  Empty : SnocList ty
  Snoc : (xs: SnocList ty) -> (x: ty) -> SnocList ty

reverseSnoc : SnocList ty -> List ty
reverseSnoc Empty = []
reverseSnoc (Snoc xs x) = x :: reverseSnoc xs
-}

data SnocList : List a -> Type where
  Empty : SnocList []
  Snoc  : (rec : SnocList xs) -> SnocList (xs ++ [x]) 
  
snocListHelp : (snoc : SnocList input) -> (rest : List a) -> SnocList (input ++ rest)
snocListHelp {input} snoc [] = rewrite appendNilRightNeutral input in snoc
snocListHelp {input} snoc (x :: xs) = rewrite appendAssociative input [x] xs in snocListHelp (Snoc snoc {x}) xs

snocList : (xs : List a) -> SnocList xs
snocList xs = snocListHelp Empty xs

myReverseHelper : (input : List a) -> SnocList input -> List a
myReverseHelper [] Empty = []
myReverseHelper (xs ++ [x]) (Snoc rec) = x :: myReverseHelper xs rec

myReverse : List a -> List a
myReverse input with (snocList input)
  myReverse [] | Empty = []
  myReverse (xs ++ [x]) | (Snoc rec) = x :: myReverse xs | rec





   
