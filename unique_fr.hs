mport Data.List
unique_fr xs = foldr (\x -> (\xs ->( [x] ++(delete x xs)))) [] xs
