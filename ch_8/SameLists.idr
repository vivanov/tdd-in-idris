same_lists : {xs : List a} -> {ys : List a} -> x = y -> xs = ys -> x :: xs = y :: ys 
same_lists Refl prf1 = cong prf1
