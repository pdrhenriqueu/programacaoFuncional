-- Exercicio 9
prodIntervalo :: Int -> Int -> Int
prodIntervalo m n
 |m > n = 1
 |otherwise = m * prodIntervalo(m+1) n