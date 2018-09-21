data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double
           
area: Shape -> Double           
area (Triangle base height) = 0.5 * base * height
area (Rectangle length height) = length * height
area (Circle radius) = pi * radius * radius 

data Picture = Primitive Shape
             | Combine Picture Picture
             | Rotate Double Picture
             | Translate Double Double Picture
             
pictureArea: Picture -> Double
pictureArea (Primitive shape) = area shape
pictureArea (Combine pic1 pic2) = pictureArea pic1 + pictureArea pic2
pictureArea (Rotate x pic) = pictureArea pic
pictureArea (Translate x y pic) = pictureArea pic


maxMaybe: Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing Nothing = Nothing
maxMaybe Nothing right@(Just x) = right
maxMaybe left@(Just x) Nothing = left
maxMaybe left@(Just x) right@(Just y) = case compare x y of
                                  LT => right
                                  EQ => left
                                  GT => left


biggestTriangle: Picture -> Maybe Double
biggestTriangle (Primitive shape) = case shape of
                                      triangle@(Triangle base height) => Just (area triangle)
                                      _ => Nothing
biggestTriangle (Combine pic1 pic2) = maxMaybe (biggestTriangle pic1) (biggestTriangle pic2)
biggestTriangle (Rotate _ pic) = biggestTriangle pic
biggestTriangle (Translate _ _ pic) = biggestTriangle pic


Eq Shape where
    (==) (Triangle base height) (Triangle base' height') = (base == base') && (height == height') 
    (==) (Rectangle length height) (Rectangle length' height') = (length == length') && (height == height')
    (==) (Circle radius) (Circle radius') = radius == radius'
    (==) _ _ = False
    
Ord Shape where
    compare shape1 shape2 = compare (area shape1) (area shape2)
