-- Lista 5  
-- Dara dos Santos Lima         16103611

-- 1)
membro :: Int -> [Int] -> Bool
membro a []         = False
membro a (x:xs)
      | a == x      = True
      | otherwise   = membro a xs

-- 2)
membroNum :: Int -> [Int] -> Int
membroNum a []      = 0
membroNum a (x:xs)
       | a == x      = 1 + membroNum a xs
       | otherwise   = membroNum a xs

-- 3)
membro2 :: Int -> [Int] -> Bool
membro2 a []         = False
membro2 a (x:xs)
      | (membroNum a (x:xs))> 0      = True
      | otherwise                    = False


-- 4)
unico :: [Int] -> [Int]
unico []                 = []
unico (x:xs) 
      | (membroNum x xs) == 0        = x : unico xs 
      | otherwise                    = removeIguais x (unico xs)

removeIguais :: Int -> [Int] -> [Int]
removeIguais a []       = []
removeIguais a (x:xs)
        | a /= x        = x  : removeIguais a xs
        | otherwise     = removeIguais a xs


-- 5)
quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (x:xs) = quickSort (menores x xs) ++ [x] ++ quickSort (maiores x xs)

menores :: Int -> [Int] -> [Int]
menores a []   = []
menores a (x:xs)
        | x<=a           =  x : menores a xs 
        | otherwise     = menores a xs

maiores :: Int -> [Int] -> [Int]
maiores a []   = []
maiores a (x:xs)
        | x>a           =  x : maiores a xs 
        | otherwise     = maiores a xs
