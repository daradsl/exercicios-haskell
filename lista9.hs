-- Lista 9
-- Dara dos Santos Lima         16103611

-- 1)
head2 :: [a] -> a
head2 [] = error "Lista vazia"
head2 (x:xs) = x

tail2 :: [a] -> [a]
tail2 [] = error "Lista vazia"
tail2 (x:xs) = xs

fst2 :: (a,b) -> a
fst2 (x, y) = x

shift2 :: ((a, b), c) -> (a, (b, c))
shift2 ((x, y), z) = (x, (y, z))


-- 2)
concatena :: [[a]] -> [a]
concatena []      = []
concatena (x:xs)  = x ++ concatena xs


-- 3)
inverte :: [a] -> [a]
inverte []       = []
inverte (x:xs)   = (inverte xs) ++ [x]


-- 4)
zipp3 :: [a] -> [b] -> [c] -> [(a,b,c)]
zipp3 [] x y = []
zipp3 x [] y = []
zipp3 x y [] = []
zipp3 (x:xs) (y:ys) (z:zs) = (x,y,z) : zipp3 xs ys zs


-- 5)
mapMaisUm :: (Int -> a) -> [Int] -> [a]
mapMaisUm f [] = []
mapMaisUm f (x:xs) = f (x+1) : mapMaisUm f xs

dobra :: Int -> Int
dobra x = x*x

incrementa :: Int -> Int
incrementa x = x+1

-- 6)
foldr2 :: (a -> b -> b) -> b -> [a] -> b
foldr2 f v [] = v
foldr2 f v (x:xs) = f x (foldr2 f v xs)


-- tipo da funcao: 
-- foldr :: (a->b->b) -> b -> [a] -> b
-- (funcao ou operador) -> (valor p devolver em caso de lista vazia) -> (lista) -> (saida)

-- A entrada da funcao, ou operador, eh "a" logo a lista de entrada deve ter o mesmo tipo "a"
-- para que a funcao possa ser aplicada nos elementos da lista de entrada. A saida da funcao tem tipo "b",
-- ,que pode ou nao ser igual a "a", e que tem mesmo tipo da saida de foldr ("b")
-- Entao é necessario que a lista de entrada seja do mesmo tipo da entrada da funcao que sera aplicada
-- e a saida da funcao que sera aplicada a lista, deve ser do mesmo tipo da saida da foldr em si

-- Exemplo:
-- a = Int, b = Float
-- foldr2 (+) 1.2 [5, 4, 3] 
-- 13.2

-- Neste exemplo, A entrada é uma lista de Int ([5, 4, 3]), e o valor de v é um Float (1.2)
-- O que ocorre é que em determinada execucao do operador (+)
-- Serao somados um valor Int e um Float (a + b)
-- E o resultado disto sera um Float (b)
