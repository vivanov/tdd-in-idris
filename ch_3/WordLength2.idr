import Data.Vect

total allLengths : Vect n String -> Vect n Nat
allLengths [] = []
allLengths (x :: xs) = length x :: allLengths xs

