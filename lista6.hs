-- Lista 6  
-- Dara dos Santos Lima         16103611

-- 1)
pegaPosicao :: Int -> [Int] -> Int
pegaPosicao a []        = error "Lista Vazia ou Argumento Inexistente"
pegaPosicao a (x:xs)
        | a==1          = x
        | otherwise     = pegaPosicao (a-1) xs


-- 2)
pega :: Int -> [Int] -> [Int]
pega a []             = error "Lista Vazia ou Argumento Inexistente"
pega a (x:xs)
        | a == 0      = [] 
        | a == 1      = [x]       
        | otherwise   = x : pega (a-1) xs


-- 3)
retira :: Int -> [Int] -> [Int]
retira a []            = []
retira a (x:xs)  
        | a <= 0       = x : retira (a-1) xs
        | otherwise    = retira (a-1) xs


-- 4)
mediaLista :: [Float] -> Float
mediaLista []    = 0
mediaLista x     = ((somaTotal x)/(elementos x))

somaTotal :: [Float] -> Float
somaTotal []     = 0
somaTotal (x:xs) = (x + somaTotal xs)

elementos :: [Float] -> Float
elementos []     = 0
elementos (x:xs) = 1 + elementos xs 


-- 5)
pegaMaiores :: Int -> [Int] -> [Int]
pegaMaiores a [] = []
pegaMaiores a (x:xs)
        | x > a      = x : pegaMaiores a xs 
        | otherwise  = pegaMaiores a xs


-- 6)
elementos2 :: [Int] -> Int
elementos2 []     = 0
elementos2 (x:xs) = 1 + elementos2 xs

contaMaiores :: Int -> [Int] -> Int
contaMaiores n [] = 0
contaMaiores n x = elementos2(pegaMaiores n x) 


-- 7)
intercala :: [Int] -> [Int] -> [Int]
intercala [] [] = []
intercala (x:xs) [] = (x:xs)
intercala [] (z:zs) = (z:zs)
intercala (x:xs) (z:zs) = x:z:intercala xs zs


-- 8)
dupli :: [Int] -> [Int]
dupli [] = []
dupli (x:xs) = x:x:dupli xs


-- 9)
repli :: Int -> [Char] -> [Char]
repli a [] = []
repli a (x:xs) = (repliUmPorUm a x) ++ (repli a xs)

    where
        repliUmPorUm :: Int -> Char -> [Char]
        repliUmPorUm a c
           | a == 0        = []
           | a == 1        = [c] 
           | otherwise     = c : repliUmPorUm (a-1) c


-- 10)
drobEvery :: Int -> [Char] -> [Char]
drobEvery a [] = []
drobEvery a (x:xs) = corta a (x:xs) ++ drobEvery (a) (restante a (x:xs))

    where
        corta :: Int -> [Char] -> [Char]
        corta n [] = []
        corta n (x:xs)
            | n == 1      = corta n xs
            | otherwise   = x : corta (n-1) xs 

        restante :: Int -> [Char] -> [Char]
        restante a [] = []
        restante a (x:xs)
            | a <= 0       = x : restante (a-1) xs
            | otherwise    = restante (a-1) xs
                     

-- 11)
split :: Int -> [Char] -> ([Char], [Char])
split a [] = ([],[])
split a x = ((primeiro a x), (segundo a x))

    where
        primeiro :: Int -> [Char] -> [Char]
        primeiro n [] = []
        primeiro n (x:xs)
            | n == 1       = [x]
            | otherwise    = x : primeiro (n-1) xs

        segundo :: Int -> [Char] -> [Char] 
        segundo n [] = []
        segundo n (x:xs)
            | n <= 0       = x : segundo (n-1) xs
            | otherwise    = segundo (n-1) xs
