palyndrome: Nat -> String -> Bool
palyndrome n xs = if length xs > n then toLower (reverse xs) == (toLower xs) else False
