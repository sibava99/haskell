prefixList_fr [] = []
prefixList_fr xs = prefixList_fr(init xs) ++ [(foldr (\x -> (\xs -> [x] ++ xs)) [] xs)]

                   
