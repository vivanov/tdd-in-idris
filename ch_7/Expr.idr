data Expr : Type -> Type where
  Val  :  num  -> (Expr num)
  Add  : (Expr num) -> (Expr num) -> (Expr num)
  Sub  : (Expr num) -> (Expr num) -> (Expr num)
  Mul : (Expr num) -> (Expr num) -> (Expr num)
  Div  : (Expr num) -> (Expr num) -> (Expr num)
  Abs  : (Expr num) -> (Expr num)
  

total eval: (Neg num, Integral num) => Expr num -> num
eval (Val x)    = x
eval (Add x y)  = eval x + eval y
eval (Sub x y)  = eval x - eval y
eval (Mul x y) = eval x * eval y
eval (Div x y)  = eval x `div` eval y
eval (Abs x)    = abs (eval x)

Num ty => Num (Expr ty) where
    (+) = Add
    (*) = Mul
    fromInteger = Val . fromInteger
    
Neg ty => Neg (Expr ty) where
    negate x = 0 - x
    (-) = Sub
    abs = Abs
    
Show ty => Show (Expr ty) where     
    show (Val x) = show x
    show (Add x y) = "(" ++ (show x) ++ " + " ++ (show y) ++ ")"
    show (Sub x y) = "(" ++ (show x) ++ " - " ++ (show y) ++ ")"
    show (Mul x y) = "(" ++ (show x) ++ " * " ++ (show y) ++ ")"
    show (Div x y) = "(" ++ (show x) ++ " / " ++ (show y) ++ ")"
    show (Abs x) = "(|" ++ (show x) ++ "|)"
    
(Integral ty, Neg ty, Eq ty) => Eq (Expr ty) where
    (==) x y = (eval x) == (eval y)

(Integral ty, Neg ty) => Cast (Expr ty) ty where
    cast orig = eval orig

Functor Expr where
    map func (Val x) = Val (func x)
    map func (Add x y) = Add (map func x) (map func y) 
    map func (Sub x y) = Sub (map func x) (map func y) 
    map func (Mul x y) = Mul (map func x) (map func y) 
    map func (Div x y) = Div (map func x) (map func y) 
    map func (Abs x) = Abs (map func x)
