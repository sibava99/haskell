data Tree a = Node (Tree a) a (Tree a) | Empty
  deriving (Eq,Ord,Show,Read)


treeFold:: (t1 -> t -> t1 ->t1) ->t1 -> Tree t ->t1
treeFold _ z Empty = z
treeFold f z (Node l c r) = f  (treeFold f z l) c (treeFold f z r)  


occursF x = treeFold (\l -> \y -> \r -> x == y || l || r) False

nodesF  = treeFold(\l -> \y -> \r -> 1 + l + r) 0

toPreOrderF = treeFold(\l -> \y -> \r -> y ++ "("++ l++")" ++"("++ r++")")""
