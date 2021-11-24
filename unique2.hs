unique2 [] = []
unique2 (x:xs) = if elem x xs then unique2 xs else x:unique2 xs
