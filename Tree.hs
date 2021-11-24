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
maxTree (Node l x r) = maximum [maxTree l, Just x,maxTree r]

treeFold:: (t1 -> t -> t1 ->t1) ->t1 -> Tree t ->t1
treeFold _ z Empty = z
treeFold f z (Node l c r) = f  (treeFold f z l) c (treeFold f z r)  


occursF x = treeFold (\l -> \y -> \r -> x == y || l || r) False

nodesF  = treeFold(\l -> \y -> \r -> 1 + l + r) 0

toPreOrderF = treeFold(\l -> \y -> \r -> y ++ "("++ l++")" ++"("++ r++")")""
