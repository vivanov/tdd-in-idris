data Expr: Type where
  Val:  Int  -> Expr
  Add:  Expr -> Expr -> Expr
  Sub:  Expr -> Expr -> Expr
  Mult: Expr -> Expr -> Expr
  

total evaluate: Expr -> Int
evaluate (Val x)   = x
evaluate (Add x y) = let x1 = evaluate x
                         y1 = evaluate y
                     in x1 + y1    
evaluate (Sub x y) = let x1 = evaluate x
                         y1 = evaluate y
                     in x1 - y1    
evaluate (Mult x y) = let x1 = evaluate x
                          y1 = evaluate y
                      in x1 * y1    
