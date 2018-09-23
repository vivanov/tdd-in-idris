import Data.Primitives.Views
import System

% default total

data Command : Type -> Type where
  PutStr : String -> Command ()
  GetLine : Command String

data ConsoleIO : Type -> Type where
  Quit : a -> ConsoleIO a
  Do : Command a -> (a -> Inf (ConsoleIO  b)) -> ConsoleIO b
  
(>>=) : Command a -> (a -> Inf (ConsoleIO  b)) -> ConsoleIO b
(>>=) = Do

runCommand : Command a -> IO a
runCommand (PutStr x) = putStr x
runCommand GetLine = getLine

data Fuel : Type where
  Dry : Fuel
  More : (Lazy Fuel) -> Fuel

partial
forever : Fuel
forever = More forever

run : Fuel -> ConsoleIO a -> IO (Maybe a)
run _ (Quit val) = pure (Just val)
run Dry _ = pure (Nothing)
run (More fuel) (Do cmd cont) = do res <- runCommand cmd
                                   run fuel (cont res)

quiz : Stream Int -> (score : Nat) -> ConsoleIO Nat
quiz (num1 :: num2 :: nums) score =
  do PutStr ("Score so far: " ++ show score ++ "\n")
     PutStr (show num1 ++ " * " ++ show num2 ++ "? ")
     answer <- GetLine
     if toLower answer == "quit" then Quit score else
       if (cast answer == num1 * num2)
         then do PutStr "Correct!\n"
                 quiz nums (score + 1)
         else do PutStr ("Wrong, the anwer is " ++ show (num1 * num2) ++ "\n")
                 quiz nums score

randoms : Int -> Stream Int
randoms seed = let seed' = 1664525 * seed + 1013904223 in
                   (seed' `shiftR` 2) :: randoms seed'                 

arithInputs : Int -> Stream Int
arithInputs seed = map bound (randoms seed)
  where bound : Int -> Int
        bound num with (divides num 9)
          bound ((9 * div) + rem) | (DivBy prf) = rem + 1

partial
main : IO ()
main = do seed <- time
          Just score <- run forever (quiz (arithInputs (fromInteger seed)) 0)
              | Nothing => putStrLn "Ran out of fuel"
          putStrLn ("Final score: " ++ show score)

mutual
  correct : Stream Int -> (score : Nat) -> ConsoleIO Nat
  correct nums score = do PutStr "Correct!\n"
                          quiz2 nums (score + 1)
  
  wrong : Stream Int -> Int -> (score : Nat) -> ConsoleIO Nat
  wrong nums ans score = do PutStr ("Wrong, the answer is " ++ show ans ++ "\n")
                            quiz2 nums score
                            
                            
  
  quiz2 : Stream Int -> (score : Nat) -> ConsoleIO Nat
  quiz2 (num1 :: num2 :: nums) score =
    do PutStr ("Score so far: " ++ show score ++ "\n")
       PutStr (show num1 ++ " * " ++ show num2 ++ "? ")
       answer <- GetLine
       if toLower answer == "quit" then Quit score else
         if (cast answer == num1 * num2)
           then correct nums score
           else wrong nums (num1 * num2) score
