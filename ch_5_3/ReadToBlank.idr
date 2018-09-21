readToBlank : IO (List String)
readToBlank = do
                x  <- getLine
                if x == ""
                  then pure([])
                  else do xs <- readToBlank 
                          pure(x :: xs)

readAndSave : IO ()
readAndSave = do
               lines <- readToBlank
               putStrLn "Please enter name of the output file"
               name  <- getLine
               Right file <- openFile name Append
                    | Left err => do putStrLn ("Error during file opening: " ++ show err)
               Right _ <- writeFile name (unlines lines)
                    | Left err => do putStrLn ("Error during file closing: " ++ show err)
               _ <- closeFile file
               pure ()                   

main : IO ()
main = do putStrLn "Please input any number of lines, empty line terminate program:"
          _ <- readAndSave
          putStrLn "Input has been saved to file"                

