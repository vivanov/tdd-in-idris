data MyList: (elem: Type) -> Type where
  MyNil  : MyList elem
  MyCons : elem -> MyList elem -> MyList elem


data Elem : a -> MyList a -> Type where
  Here  : Elem x (MyCons x xs)
  There : (later: Elem x xs) -> Elem x (MyCons y xs) 


data Last : List a -> a -> Type where
  LastOne  : Last [value] value
  LastCons : (prf : Last xs value) -> Last (x :: xs) value

notLast : (absent : (x = value) -> Void) -> Last [x] value -> Void
notLast absent LastOne = absent Refl
notLast _ (LastCons LastOne) impossible
notLast _ (LastCons (LastCons _)) impossible

Uninhabited (Last [] value) where
  uninhabited LastOne impossible
  uninhabited (LastCons _) impossible

{-
notInNil : Last [] value -> Void
notInNil LastOne impossible
notInNil (LastCons _) impossible
-}

notInTail : (notLastCons : Last (y :: xs) value -> Void) -> Last (x :: (y :: xs)) value -> Void
notInTail notLastCons (LastCons later) = notLastCons later

isLast : DecEq a => (xs : List a) -> (value : a) -> Dec (Last xs value)
isLast [] value = No uninhabited
isLast (x :: []) value = case decEq x value of
                              Yes Refl   => Yes LastOne
                              No absent  => No (notLast absent) 
isLast (x :: (y :: xs)) value =  case isLast (y :: xs) value of
                                   Yes prf => Yes (LastCons prf)
                                   No notLastCons => No (notInTail notLastCons) 
 
