import Data.Primitives.Views

quiz : Stream Int -> (score : Nat) -> IO ()
quiz (num1 :: num2 :: numz) score
  = do putStrLn ("Score so far: " ++ show score)
       putStr (show num1 ++ " * " ++ show num2 ++ "? ")
       answer <- getLine
       if cast answer == num1 * num2
         then do putStrLn "Correct!"
                 quiz numz (score + 1)
         else do putStrLn("Wrong, the answer is " ++ show (num1 * num2))
                 quiz numz score
                 
randoms : Int -> Stream Int
randoms seed = let seed' = 1664525 * seed + 1013904223 in
                   (seed' `shiftR` 2) :: randoms seed'                 

arithInputs : Int -> Stream Int
arithInputs seed = map bound (randoms seed)
  where bound : Int -> Int
        bound num with (divides num 9)
          bound ((9 * div) + rem) | (DivBy prf) = rem + 1
