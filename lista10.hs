-- Lista 10  
-- Dara dos Santos Lima         16103611


-- 1)
concatena :: [[a]] -> [a]
concatena [] = []
concatena (x:xs) = (foldr (++) [] (x:xs))


-- 2)
andLista :: [Bool] -> Bool
andLista [x] = x
andLista (x:xs) = (foldr (&&) True (x:xs))


-- 3)
somaQuadPos :: [Int] -> Int
somaQuadPos [] = 0
somaQuadPos (x:xs) = foldr (+) 0 (map (^2) (filter (>(-1)) (x:xs)))


-- 4)
somaListas :: [[Int]] -> Int
somaListas [] = 0
somaListas (x:xs) = foldr (+) 0 (map (soma) (x:xs))
    where
        soma :: [Int] -> Int
        soma [] = 0
        soma (x:xs) = x + soma xs


-- 5)
tamanhoListas :: [[a]] -> Int 
tamanhoListas [] = 0
tamanhoListas (x:xs) = foldr (+) 0 (map (tamanho) (x:xs)) 
    where 
        tamanho :: [a] -> Int
        tamanho [] = 0
        tamanho (x:xs) = 1 + tamanho xs


-- 6)
inverte :: [a] -> [a]
inverte []       = []
inverte (x:xs)   = foldr (invertendo) [] (x:xs)
    where
        invertendo :: a -> [a] -> [a]
        invertendo a b = b ++ [a]


-- 7)
separaPalavras :: [Char] -> [[Char]]
separaPalavras [] = []
separaPalavras (x:xs) = takeWhile (/= ' ') (x:xs) : separaPalavras (dropWhile (== ' ') (dropWhile (/= ' ') (x:xs)))
