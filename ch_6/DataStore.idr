import Data.Vect

infixr 5 .+.

data Schema = SString
            | SInt
            | SChar
            |  (.+.) Schema Schema

SchemaType: Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType SChar = Char
SchemaType (x .+. y) = (SchemaType x, SchemaType y) 


record DataStore where
       constructor MkData
       schema : Schema
       size : Nat
       items : Vect size (SchemaType schema)



data Command : Schema -> Type where 
             SetSchema : (newschema : Schema) -> Command schema
             Add  :  SchemaType schema -> Command schema
             Get  : Integer -> Command schema
             Size : Command schema
             ListAll : Command schema
             Quit : Command schema
  


addToStore : (store : DataStore) -> SchemaType (schema store)  -> DataStore
addToStore (MkData schema size items) newitem = MkData schema _ (addToData items)
  where
    addToData : Vect old (SchemaType schema) -> Vect (S old) (SchemaType schema)
    addToData [] = [newitem]
    addToData (olditem :: olditems) = olditem :: addToData olditems
    

parsePrefix : (schema : Schema) -> String -> Maybe(SchemaType schema, String)
parsePrefix SChar   input = case unpack (input) of
                                 (x :: xs) => Just(x, (ltrim (pack xs)))
                                 _         => Nothing
parsePrefix SString input = getQuoted (unpack input)
                               where getQuoted : List Char -> Maybe (String, String)
                                     getQuoted ('"' :: xs) = 
                                         case span (/= '"') xs of
                                              (quoted, '"' :: rest) => Just (pack quoted, ltrim (pack rest))
                                              _                  => Nothing
                                     getQuoted _           = Nothing 
parsePrefix SInt input = case span isDigit input of
                              ("", rest)  => Nothing
                              (num, rest) => Just(cast num, ltrim rest)
parsePrefix (schemal .+. schemar) input = case parsePrefix schemal input of
                                               Nothing             => Nothing
                                               Just(l_val, input') => 
                                                    case parsePrefix schemar input' of
                                                         Nothing => Nothing
                                                         Just(r_val, input'') =>
                                                              Just((l_val, r_val), input'')

parseBySchema : (schema: Schema) -> String -> Maybe (SchemaType schema)
parseBySchema schema input = case parsePrefix schema input of
                                  Nothing => Nothing
                                  Just (res, "") => Just res
                                  Just _  => Nothing


mutual
  parseSchema : List String -> Maybe Schema
  parseSchema ("Char" :: xs) = parseRemaining SChar xs
  parseSchema ("String" :: xs) = parseRemaining SString xs
  parseSchema ("Int" :: xs) = parseRemaining SInt xs
  parseSchema _ = Nothing


  parseRemaining : (schema: Schema) -> List String -> Maybe Schema
  parseRemaining schema [] = Just schema
  parseRemaining schema xs = do xs_sch <- parseSchema xs
                                Just (schema .+. xs_sch)


parseCommand : (schema : Schema) -> (cmd : String) -> (args : String) -> Maybe (Command schema)
parseCommand schema "add" str     = case parseBySchema schema str of
                                         Nothing   => Nothing
                                         something => map Add something
parseCommand schema "get" pos with (pos)
                                   | "" = Just ListAll
                                   | _  = case all isDigit (unpack pos) of
                                                  False => Nothing
                                                  True => Just (Get (cast pos))
parseCommand schema "size" ""     = Just Size
parseCommand schema "quit" ""     = Just Quit
parseCommand schema "schema" rest = do sch <- parseSchema (words rest)
                                       Just(SetSchema sch)
parseCommand _ _ _           = Nothing

parse : (schema : Schema) -> (input : String) -> Maybe (Command schema)
parse schema input = case span (/= ' ') input of
                          (cmd, args) => parseCommand schema cmd (ltrim args)

display : SchemaType schema -> String
display {schema = SChar} item = show item
display {schema = SString} item = item
display {schema = SInt} item = show item
display {schema = (x .+. y)} (iteml, itemr) = display iteml ++ ", " ++ display itemr


getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store = let store_items = items store in
                          case (integerToFin pos (size store)) of
                               Nothing => Just ("Out of range\n", store)
                               Just id => Just (display (index id store_items) ++ "\n", store) 

setSchema : (store : DataStore) -> Schema -> Maybe DataStore
setSchema store schema = case size store of
                              Z   => Just (MkData schema _ [])
                              S k => Nothing


record Entry where
       constructor MkEntry
       index : Nat
       value : String

entriesList : (n : Nat) -> (allItems: Vect n (SchemaType store_schema)) -> List Entry
entriesList Z [] = []
entriesList (S k) (x :: xs) = MkEntry k (display x) :: entriesList k xs


processCommand : (store : DataStore) ->  (command : Command (schema store)) -> Maybe (String, DataStore)
processCommand store (Add item)  = Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
processCommand store (Get pos)   = getEntry pos store
processCommand store Size        = Just (show (size store) ++ "\n", store)
processCommand store (SetSchema newschema) = case (setSchema store newschema) of
                                                   Nothing => Just("Unable to update schema\n", store)
                                                   Just newstore => Just("OK\n", newstore)
processCommand store ListAll     = let store_items = items store
                                       store_schema = schema store
                                       entries = entriesList _ store_items
                                       show_entries = (showEntries entries) ++ "\n" in
                                       Just (show_entries, store)
                                         where 
                                           showEntry : Entry -> String
                                           showEntry entry = (show . index $ entry) ++ ": " ++ (value $ entry) ++ "\n"
                                           showEntries entries = concat . (map showEntry) $ entries
                                             
                                             
processCommand _ Quit              = Nothing


processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input = case (parse (schema store) input) of
                                Nothing => Just ("Invalid command\n", store)
                                Just command => processCommand store command
                                            
main : IO ()
{- 
  Schema below is just an initial value, actual schema 
  is set by issuing schema command on empty store 
-}
main = replWith (MkData (SString .+. SString .+. SInt) _ []) "Command: " processInput 
    
  
  
  
