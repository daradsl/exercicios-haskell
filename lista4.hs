-- Lista de exercicios 4
-- Dara dos Santos Lima                 16103611

-- 1)
multDoisLista :: [Int] -> [Int]
multDoisLista []       = []
multDoisLista (x:xs)   = 2*x : multDoisLista xs

-- 2)
tamanho :: [Int] -> Int
tamanho []       = 0
tamanho (x:xs)   = 1 + tamanho xs

--3)
produtoLista :: [Int] -> Int
produtoLista []       = 1
produtoLista (x:xs)   = x * produtoLista xs

--4)
andLista :: [Bool] -> Bool
andLista []       = True
andLista (x:xs)   = x && andLista xs

--5)
concatLista :: [[Int]] -> [Int]
concatLista []         = []
concatLista (x:xs)     = x ++ concatLista xs

--6)
inverteLista :: [Int] -> [Int]
inverteLista []       = []
inverteLista (x:xs)   = (inverteLista xs) ++ [x]
