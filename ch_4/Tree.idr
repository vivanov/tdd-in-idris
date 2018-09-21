data Tree elem = Empty
            | Node (Tree elem) elem (Tree elem)

%name Tree tree, tree1

insert: Ord elem => elem -> Tree elem -> Tree elem
insert x Empty = Node Empty x Empty
insert x orig@(Node left val right) = case compare x val of
                                      LT => Node (insert x left) val right
                                      EQ => orig
                                      GT => Node left val (insert x right)


listToTree: Ord a => List a -> Tree a
listToTree [] = Empty
listToTree (x :: xs) = insert x (listToTree xs)

append: List a -> List a -> List a
append [] ys = ys
append (x :: xs) ys = x :: append xs ys


treeToList: Tree a -> List a
treeToList Empty = []
treeToList (Node left val right) = let leftList = treeToList left
                                       rightList = treeToList right
                                       singleton = [val]
                                       leftAppended = append leftList singleton
                                   in  append leftAppended rightList  
                                  

