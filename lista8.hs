-- Lista 8
-- Dara dos Santos Lima         16103611

-- 1)
aplicaDuasVezes :: (Int->Int) -> Int -> Int
aplicaDuasVezes f x = (f (f x))


dobra :: Int -> Int
dobra x = x*x

incrementa :: Int -> Int
incrementa x = x + 1

-- 2)
vendas :: Int -> Int
vendas 0 = 10
vendas 1 = 5
vendas 2 = 6
vendas 3 = 5
vendas _ = 10

vendaTotal :: (Int -> Int) -> Int -> Int
vendaTotal f 0 = f 0
vendaTotal f n = f n + vendaTotal f (n-1)


-- 3)
foldInt :: (Int->Int->Int) -> [Int] -> Int
foldInt f [] = error "erro"
foldInt f [x] = x
foldInt f (x:xs) = f (x) (foldInt f xs)

soma :: Int -> Int -> Int
soma x y = x + y

mult :: Int -> Int -> Int
mult x y = x * y


-- 4)
filterString :: (Char -> Bool) -> [Char] -> [Char]
filterString f [] = []
filterString f (x:xs)
    | (f (x)) == True      = x : filterString f xs
    | otherwise            = filterString f xs

naoEspaco :: Char -> Bool
naoEspaco x = x /= ' '


-- 5)
somaQuadrado :: [Int] -> Int
somaQuadrado [] = 0
somaQuadrado [x] = dobra x
somaQuadrado (x:xs) = foldInt (soma) (mapInt (dobra) (x:xs))

mapInt :: (Int -> Int) -> [Int] -> [Int]
mapInt f [] = []
mapInt f (x:xs) = f x : mapInt f xs


-- 6)
iter :: Int -> (Int->Int) -> Int -> Int
iter 0 f x = error "erro"
iter 1 f x = f x
iter n f x = f (iter (n-1) f x)
