-- Exercicio 7
passo :: (Int,Int) -> (Int,Int)
passo(x,y) = (y,y+x)

fibo :: Int -> (Int,Int)

fibo 0 = (0,1)
fibo n = (fibo (n-1))