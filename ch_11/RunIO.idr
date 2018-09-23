% default total

data RunIO : Type -> Type where
  Quit : a -> RunIO a
  Do : IO a -> (a -> Inf (RunIO b)) -> RunIO b
  
(>>=) : IO a -> (a -> Inf (RunIO b)) -> RunIO b
(>>=) = Do

data Fuel : Type where
  Dry : Fuel
  More : (Lazy Fuel) -> Fuel

partial
forever : Fuel
forever = More forever

greet : RunIO ()
greet = do putStr "Enter your name: "
           name <- getLine
           if name == ""
             then do putStrLn "Bye bye!"
                     Quit()
             else do putStrLn ("Hello " ++ name)
                     greet
                   
run : Fuel -> RunIO a -> IO (Maybe a)
run _ (Quit val) = pure (Just val)
run Dry _ = pure Nothing
run (More fuel) (Do a c) = do res <- a
                              run fuel (c res)


partial 
main : IO ()
main = do run forever greet
          pure ()
