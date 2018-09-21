import Data.Vect

data Command = Add String
             | Get Integer
             | Size
             | Search String
             | Quit

data DataStore : Type where
  MkData : (size : Nat) -> (items : Vect size String) -> DataStore
  
size : DataStore -> Nat
size (MkData size' items') = size'

items : (store : DataStore) -> Vect (size store) String
items (MkData size' items') = items'

addToStore : DataStore -> String -> DataStore
addToStore (MkData size' items) newitem = MkData _ (addToData items)
  where
    addToData : Vect old String -> Vect (S old) String
    addToData [] = [newitem]
    addToData (olditem :: olditems) = olditem :: addToData olditems
    
parseCommand : (cmd : String) -> (args : String) -> Maybe Command
parseCommand "add" str     = Just (Add str)
parseCommand "get" pos     = case all isDigit (unpack pos) of
                              False => Nothing
                              True => Just (Get (cast pos))
parseCommand "size" ""     = Just Size
parseCommand "search" prfx = Just (Search prfx)
parseCommand "quit" ""     = Just Quit
parseCommand _ _           = Nothing

parse : String -> Maybe Command
parse input = case span (/= ' ') input of
                   (cmd, args) => parseCommand cmd (ltrim args)

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store = let store_items = items store in
                          case (integerToFin pos (size store)) of
                               Nothing => Just ("Out of range\n", store)
                               Just id => Just (index id store_items ++ "/n", store) 


getEntries : (prfx : String) -> (store : DataStore) -> Maybe (String, DataStore)
getEntries prfx store = let store_items = items store
                            entries = entriesList _ [] store_items prfx in
                            Just (unwords(map (\entry => show (fst entry) ++ ": " ++ (snd entry)) entries) ++ "\n", store)
                              where
                                entriesList : (n : Nat) -> (acc: List (Nat, String)) -> (allItems: Vect n String) -> (prfx: String) -> List (Nat, String)
                                entriesList Z acc [] prfx = acc
                                entriesList (S k) acc (x :: xs) prfx = case isInfixOf prfx x of
                                                                      True  => entriesList k ((k, x) :: acc) xs prfx
                                                                      False => entriesList k acc xs prfx
                                                                      
                                

processCommand : (command : Command) -> (store : DataStore) -> Maybe (String, DataStore)
processCommand (Add item) store     = Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
processCommand (Get pos) store      = getEntry pos store
processCommand Size store           = Just (show (size store) ++ "\n", store)
processCommand (Search prfx) store  = getEntries prfx store
processCommand Quit _               = Nothing


processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input = case (parse input) of
                                Nothing => Just ("Invalid command\n", store)
                                Just command => processCommand command store

main : IO ()
main = replWith (MkData _ []) "Command: " processInput

    
  
  
  
