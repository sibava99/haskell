unique [] = []
unique (x:xs) | [ c | c <- xs , c == x] == [] = x:unique xs
                | [ c | c <- xs , c == x] /= [] = unique xs
