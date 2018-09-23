data Input = Answer Int | QuitCmd

data Command : Type -> Type where
  PutStr : String -> Command()
  GetLine : Command String
  
  ReadFile : String -> Command (Either FileError String)
  WriteFile : String -> String -> Command (Either FileError ())
      
  Pure : ty -> Command ty
  Bind : Command a -> (a -> Command b) -> Command b
  
runCommand : Command a -> IO a
runCommand (PutStr x) = putStr x
runCommand GetLine = getLine
runCommand (ReadFile x) = readFile x
runCommand (WriteFile x y) = writeFile x y
runCommand (Pure x) = pure x
runCommand (Bind c f) = do res <- runCommand c
                           runCommand (f res)
                           
data ConsoleIO : Type -> Type where
  Quit : a -> ConsoleIO a
  Do : Command a -> (a -> Inf (ConsoleIO  b)) -> ConsoleIO b
  
namespace ConsoleDo
  (>>=) : Command a -> (a -> Inf (ConsoleIO  b)) -> ConsoleIO b
  (>>=) = Do

namespace CommandDo
  (>>=) : Command a -> (a -> Command b) -> Command b
  (>>=) = Bind                           

readInput : (prompt : String) -> Command Input
readInput prompt = do PutStr prompt
                      answer <- GetLine
                      if toLower answer == "quit"
                        then Pure QuitCmd
                        else Pure (Answer (cast answer))

mutual
  correct : Stream Int -> (score : Nat) -> ConsoleIO Nat
  correct nums score = do PutStr "Correct!\n"
                          quiz nums (score + 1)
  
  wrong : Stream Int -> Int -> (score : Nat) -> ConsoleIO Nat
  wrong nums ans score = do PutStr ("Wrong, the answer is " ++ show ans ++ "\n")
                            quiz nums score
                            
                            
  
  quiz : Stream Int -> (score : Nat) -> ConsoleIO Nat
  quiz (num1 :: num2 :: nums) score =
    do PutStr ("Score so far: " ++ show score ++ "\n")
       input <- readInput (show num1 ++ " * " ++ show num2 ++ "? ")
       case input of
         Answer answer => if answer == num1 * num2
                            then correct nums score
                            else wrong nums (num1 * num2) score
         QuitCmd => Quit score


mutual
  correct2 : Stream Int -> (score : Nat) -> (attempts : Nat) -> ConsoleIO Nat
  correct2 nums score attempts = do PutStr "Correct!\n"
                                    quiz2 nums (score + 1) (attempts + 1)
  
  wrong2 : Stream Int -> Int -> (score : Nat) ->  (attempts : Nat) -> ConsoleIO Nat
  wrong2 nums ans score attempts = do PutStr ("Wrong, the answer is " ++ show ans ++ "\n")
                                      quiz2 nums score (attempts + 1)

  quiz2 : Stream Int -> (score : Nat) -> (attempts: Nat) -> ConsoleIO Nat
  quiz2 (num1 :: num2 :: nums) score attempts =
    do PutStr ("Score so far: " ++ show score ++ "/" ++ show attempts ++ "\n")
       input <- readInput (show num1 ++ " * " ++ show num2 ++ "? ")
       case input of
         Answer answer => if answer == num1 * num2
                            then correct2 nums score attempts
                            else wrong2 nums (num1 * num2) score attempts
         QuitCmd => Quit score

                                                   
data CommandType : Type where
  ExitCmd : CommandType
  CatCmd : (filename : String) -> CommandType
  CopyCmd : (source: String) -> (desination: String) -> CommandType

readShell : (prompt: String) -> Command CommandType
readShell prompt = do PutStr prompt
                      command <- GetLine
                      let splitted = split (== ' ') (toLower command)
                      case splitted of
                        ("exit" :: Nil) => Pure ExitCmd
                        ("cat" :: filename :: Nil)  => Pure (CatCmd filename)
                        ("copy" :: source :: destination :: Nil) => Pure (CopyCmd source destination)
                              
mutual                     
  cat : String -> ConsoleIO ()
  cat filename = do 
                   Right content <- ReadFile filename | Left fileError => do PutStr ("Error reading file: " ++ show fileError)
                                                                             shell
                   PutStr ("Content of the file:\n" ++ content)
                   shell

  copy : String -> String -> ConsoleIO ()
  copy source destination = do
                              contentOrError <- ReadFile source
                              case contentOrError of
                                Left fileError => do PutStr ("Error reading file: " ++ show fileError)
                                                     shell
                                Right content  => do
                                                     resultOrError <- WriteFile destination content
                                                     case resultOrError of
                                                       Left fileError => do PutStr ("Error writing file: " ++ show fileError)                                                              
                                                                            shell
                                                       Right _ => do PutStr ("Source " ++ source ++ " has been successfully copied to " ++ destination)
                                                                     shell


  shell : ConsoleIO ()
  shell = do
            input <- readShell "Enter one of the commands: 1) exit, 2) cat filename, 3) copy source destination\n"
            case input of
              CatCmd filename => cat filename
              CopyCmd source destination => copy source destination
              ExitCmd => Quit ()
