prefixList [] = []
prefixList k = [x | n <- [1..length k] ,x <- [take n k]]
