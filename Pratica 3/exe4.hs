-- Exercicio 4
fatorialGuardas::Int->Int
fatorialGuardas n
   |n == 0 = 1
   |otherwise = n * fatorialGuardas(n-1)

fatorialCP:: Int->Int
fatorialCP 0 = 1
fatorialCP n = n * fatorialCP(n-1)