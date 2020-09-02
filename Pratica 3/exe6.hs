-- Exercicio 6
numTri :: Int->Int
numTri n
	|n == 0 = 1
	|n == 1 = 1
	|otherwise =  n + numTri(n-1)