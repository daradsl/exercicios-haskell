-- Lista de exercicios 2
-- Dara dos Santos Lima                 16103611

-- 1)
maxi :: Int -> Int -> Int
maxi a b
    | a>b        = a
    | otherwise  = b


-- 2)
vendas :: Int -> Int
vendas 0  = 10
vendas 1  = 20
vendas 2  = 0
vendas 3  = 7
vendas 4  = 50
vendas 5  = 0
vendas 6  = 75
vendas _  = 10

maiorVenda :: Int -> Int
maiorVenda 0    = vendas 0 
maiorVenda n    = maxi (vendas n) (maiorVenda (n-1))


-- 3)
maxVenda :: Int -> Int
maxVenda n
    | maiorVenda(n) == vendas n      = n
    | otherwise                      = maxVenda(n-1)


-- 4)
zeroVendas :: Int -> Int
zeroVendas n
    | vendas(n) == 0                = n
    |(n == 0)                       = -1
    | otherwise                     = zeroVendas(n-1)


-- 5)
achaSemana :: Int -> Int -> Int
achaSemana s n
    | vendas(n) == s         = n
    |(n == 0)                = -1
    | otherwise              = achaSemana(s) (n-1)


-- 6)
zeroVendas2 :: Int -> Int
zeroVendas2 n = achaSemana 0 n


-- 7) 
-- OBS: A ordem de entrada adotada é: m n, onde m < n    ex. maiorVenda2 3 6
maiorVenda2 :: Int -> Int -> Int 
maiorVenda2 m n
    | n < m         = vendas m
    | otherwise     = maxi (vendas n) (maiorVenda2 (m)(n-1))

maxVenda2 :: Int -> Int -> Int
maxVenda2 m n
    | maiorVenda2 m n == vendas n      = n
    | otherwise                        = maxVenda2 (m) (n-1)

achaSemana2 :: Int -> Int -> Int -> Int
achaSemana2 s m n 
    | vendas(n) == s                = n
    | (n == m)                      = -1
    | otherwise                     = achaSemana2(s) (m) (n-1)

zeroVendas3 :: Int -> Int -> Int
zeroVendas3 m n = achaSemana2 0 m n


-- 8)
fatorial :: Int -> Int
fatorial 0 = 1
fatorial n = n * fatorial (n-1)


-- 9) ORDEM DE ENTRADA: M N , M<N   ex. funcao 2 6
funcao :: Int -> Int -> Int
funcao m n
    | m == n        = m
    | otherwise     = m * funcao (m+1) (n)


-- 10)
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib(n-1) + fib(n-2)
