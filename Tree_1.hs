data Tree a = Node (Tree a) a (Tree a) | Empty
  deriving (Eq,Ord,Show,Read)

occurs:: Eq a => a-> Tree a -> Bool
occurs i Empty = False
occurs i (Node l x r) = x == i || occurs i l || occurs i r


nodes:: Num a => Tree t -> a
nodes Empty = 0
nodes (Node l x r) = 1 + (nodes l) + (nodes r) 

maxTree:: Ord a => Tree a -> Maybe a
maxTree Empty = Nothing
maxTree (Node l x r) = (maximum [maxTree l, Just x,maxTree r])


