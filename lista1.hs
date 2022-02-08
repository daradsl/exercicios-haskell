-- Lista de exercicios 1
-- Dara dos Santos Lima                 16103611

-- 1)
osQuatroSaoIguais :: Int -> Int -> Int -> Int -> Bool
osQuatroSaoIguais a b c d = (a == b) && (b==c) && (c==d)

-- 2)
quantosSaoIguais :: Int -> Int -> Int -> Int
quantosSaoIguais a b c   
         | (a == b) && (b == c) = 3
         | (a /= b) && (b /= c) && (a /= c) = 0
         | otherwise = 2

-- 3)
todosDiferentes :: Int -> Int -> Int -> Bool
todosDiferentes a b c = (a /= b) && (b /= c) && (a/=c)

-- 4) Faltou realizar o teste (n /= p)
-- O correto seria:  todosDiferentes n m p = (n /= m) && (m /= p) && (n /= p)

-- 5)
quantosSaoIguais2 :: Int -> Int -> Int -> Int
quantosSaoIguais2 a b c
            | todosDiferentes a b c = 0
            | osQuatroSaoIguais a b c c = 3
            | otherwise = 2

-- 6)
elevadoDois :: Int -> Int
elevadoDois n = (n * n)

-- 7)
elevadoQuatro :: Int -> Int
elevadoQuatro n = (elevadoDois n * elevadoDois n)

-- 8)
vendas :: Int -> Int
vendas 0 = 10
vendas 1 = 5
vendas 2 = 6
vendas 3 = 5
vendas _ = 10


vendaTotal :: Int -> Int
vendaTotal 0      = vendas 0
vendaTotal n      = vendas n + vendaTotal (n-1)
