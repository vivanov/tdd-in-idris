import Data.Vect

readVectLines : (file : File) -> IO (n ** Vect n String)
readVectLines file = do
                      eof <- fEOF file
                      if eof
                        then pure (_ ** [])
                        else do
                          Right line <- fGetLine file
                              | Left err => do putStrLn ("Error when reading line from file: " ++ show err)
                                               pure (_ ** [])  
                          (_ ** lines) <- readVectLines file
                          pure (_ ** line :: lines)

readVectFile : (filename : String) -> IO (n ** Vect n String)
readVectFile filename = do
                          Right file <- openFile filename Read
                              | Left err => do putStrLn ("Error during file opening: " ++ show err)
                                               pure (_ ** [])
                          lines <- readVectLines file
                          _ <- closeFile file
                          pure lines
                          
                              


