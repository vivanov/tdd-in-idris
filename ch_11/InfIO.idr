% default total

data InfIO : Type where
  Do : IO a -> (a -> Inf InfIO) -> InfIO
  
loopPrint : String -> InfIO
loopPrint msg = Do (putStrLn msg) (\_ => loopPrint msg)

partial
runPartial : InfIO -> IO ()
runPartial (Do action cont) = do
                         res <- action
                         runPartial (cont res)
                         
data Fuel : Type where
  Dry : Fuel
  More : (Lazy Fuel) -> Fuel
  
tank : Nat -> Fuel
tank Z = Dry
tank (S k) = More (tank k)

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

loopPrint2 : String -> InfIO
loopPrint2 msg = do
                   _ <- putStrLn msg
                   loopPrint2 msg
                    
