-- Exercicio 5

fibo:: Int->Int
fibo n
	|n == 1 =1
	|n == 2 =1
	|otherwise = fibo(n-2) + fibo(n-1)