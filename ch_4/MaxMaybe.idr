maxMaybe: Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing Nothing = Nothing
maxMaybe Nothing right@(Just x) = right
maxMaybe left@(Just x) Nothing = left
maxMaybe left@(Just x) right@(Just y) = case compare x y of
                                  LT => right
                                  EQ => left
                                  GT => left
