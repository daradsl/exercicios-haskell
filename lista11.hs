-- Lista 11  
-- Dara dos Santos Lima         16103611


-- 1)
data Dia = Domingo | Segunda | Terca | Quarta | Quinta | Sexta | Sabado 
    deriving(Eq,Show)


-- 2)
finalDeSemana :: Dia -> Bool
finalDeSemana Domingo = True
finalDeSemana Sabado = True
finalDeSemana _ = False


-- 3)
data TalvezFloat = Valor Float | Erro String
    deriving(Eq,Show)


-- 4)
divisao :: Float -> Float -> TalvezFloat
divisao x 0 = Erro "Divisao por zero"
divisao x y = Valor (x/y)


-- 5)
data Nat = Zero | Suc Nat
    deriving(Eq,Show)

natToint :: Nat -> Int
natToint Zero = 0
natToint (Suc n) = (1) +  natToint n 


-- 6)
intToNat :: Int -> Nat
intToNat 0 = Zero
intToNat n = (Suc (intToNat (n-1)))

