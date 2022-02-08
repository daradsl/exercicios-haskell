-- Lista 7  
-- Dara dos Santos Lima         16103611


-- 1)
somaQuadrupla :: [(Int, Int, Int, Int)] -> Int
somaQuadrupla [] = 0
somaQuadrupla ((a,b,c,d):xs) = a + b + c + d + somaQuadrupla xs


-- 2)
somaTuplas :: [((Int, Int), (Int, Int))] -> Int
somaTuplas [] = 0
somaTuplas (x:xs) = a + b + c + d + somaTuplas xs
    where
        ((a, b),(c, d)) = x


-- 3)
zipp :: [Int] -> [Int] -> [(Int,Int)]
zipp _ [] = []
zipp [] _ = []
zipp (x:xs) (z:zs) = (x, z) : zipp xs zs 


-- 4)
zipTres :: [Int] -> [Int] -> [Int] -> [(Int,Int,Int)]
zipTres _ _ [] = []
zipTres [] _ _ = []
zipTres _ [] _ = []
zipTres (x:xs) (y:ys) (z:zs) = (x, y, z) : zipTres xs ys zs


-- 5)
unZipp :: [(Int, Int)] -> ([Int],[Int])
unZipp x = (unzipEsq x, unzipDir x)

unzipDir :: [(Int, Int)] -> [Int]
unzipDir [] = []
unzipDir ((a, b):xs) = b : unzipDir xs 

unzipEsq :: [(Int, Int)] -> [Int]
unzipEsq [] = []
unzipEsq ((a, b):xs) = a : unzipEsq xs 
