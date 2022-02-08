-- Lista de exercicios 3
-- Dara dos Santos Lima                 16103611

-- 1)
somaTuplas :: ((Int, Int), (Int, Int)) -> Int
somaTuplas ((a, b), (c, d)) = (a + b + c + d)

-- 2)
shift :: ((Int, Int), Int) -> (Int, (Int, Int))
shift ((a, b), c) = (a, (b, c))

-- 3)
maxi :: Int -> Int -> Int -> Int
maxi a b c
    | (a>b) && (a>c)       = a
    | (b>a) && (b>c)       = b
    | otherwise            = c

mini :: Int -> Int -> Int -> Int
mini a b c
    | (a<b) && (a<c)        = a
    | (b<a) && (b<c)        = b
    | otherwise             = c

minEmax :: Int -> Int -> Int -> (Int, Int)
minEmax a b c = (maxi a b c, mini a b c)


-- 4)
vendas :: Int -> Int
vendas 0 = 10
vendas 1 = 5
vendas 2 = 6
vendas 3 = 0
vendas _ = 10

zeroVenda :: Int -> (Int, Bool)
zeroVenda a
    | vendas a == 0         = (a, True)
    | otherwise             = (-1, False)

-- 5)
type Livro = (String, String, Int)

sol :: Livro
sol = ("O Sol e para Todos", "Harper Lee", 4434233)

titulo :: Livro -> String
titulo (t, a, i) = t

autor :: Livro -> String
autor (t, a, i) = a

isbn :: Livro -> Int
isbn (t, a, i) = i 
