import Data.Primitives.Views
import System

% default total

data InfIO : Type where
  Do : IO a -> (a -> Inf InfIO) -> InfIO
  
data Fuel : Type where
  Dry : Fuel
  More : (Lazy Fuel) -> Fuel

run : Fuel -> InfIO -> IO ()
run Dry _ = putStrLn "Out Of Fuel"
run (More fuel) (Do action cont) = do
                                     res <- action
                                     run fuel (cont res)
partial
forever : Fuel
forever = More forever

(>>=) : IO a -> (a -> Inf InfIO) -> InfIO
(>>=) = Do

quiz : Stream Int -> (score : Nat) -> InfIO
quiz (num1 :: num2 :: numz) score = do
                                      putStrLn ("Score so far: " ++ show score)
                                      putStr (show num1 ++ " * " ++ show num2 ++ "? ")
                                      answer <- getLine
                                      if(cast answer == num1 * num2)
                                        then do putStrLn "Correct!"
                                                quiz numz (score + 1)
                                        else do putStrLn ("Wrong, the answer is " ++ show (num1 * num2))
                                                quiz numz score
                                                
randoms : Int -> Stream Int
randoms seed = let seed' = 1664525 * seed + 1013904223 in
                 (seed' `shiftR` 2) :: randoms seed'

arithInputs: Int -> Stream Int
arithInputs seed = map bound (randoms seed)
  where
    bound : Int -> Int
    bound x with (divides x 12)
      bound ((12 * div) + rem) | (DivBy prf) = abs rem + 1

partial
main : IO ()
main = do
         seed <- time
         run forever (quiz (arithInputs (fromInteger seed)) 0)

totalREPL : (prompt : String) -> (action: String -> String) -> InfIO
totalREPL prompt action = do
                            putStr prompt
                            text <- getLine
                            putStr (action text)
                            totalREPL prompt action
                            
                            
