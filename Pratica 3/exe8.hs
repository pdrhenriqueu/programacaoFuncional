-- Exercicio 8 
potencia2::Int->Int
potencia2 n
	|n == 0 = 1
	|n == 1 = 2
	|otherwise = 2 * (potencia2(n-1))
