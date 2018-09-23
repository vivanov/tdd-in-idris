square_root_approx : (number : Double) -> (approx : Double) -> Stream Double
square_root_approx number approx = let next = (approx  + (number / approx)) / 2 in
                                       approx :: square_root_approx number next
                                       
square_root_bound : (max : Nat) -> (number : Double) -> (bound : Double) -> (approxs : Stream Double) -> Double                                       
square_root_bound Z number bound (x :: _) = x
square_root_bound (S k) number bound (value :: xs) = let squared = value * value in
                                                         let diff = squared - number in
                                                             if diff < bound 
                                                               then value
                                                               else square_root_bound k number bound xs
                                                               
square_root : (number : Double) -> Double
square_root number = square_root_bound 100 number 0.00000000001 (square_root_approx number number)                                                               
