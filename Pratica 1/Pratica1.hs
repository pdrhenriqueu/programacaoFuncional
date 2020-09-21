--Exercicio 1--
{-  
1 + 2 * 3 
=> 7
5 ^ 3
=> 125
5 ** 3
=> 125.0
5 / 3
=> 1.6666666666666667
div 5 3
=> 1
mod 5 3
=> 2
5 < 3
=> False
mod 5 3 < 2
=> True
mod 5 3 == 2
=> True
sqrt 81
=> 9.0
logBase 2 1024
=> 10.0
floor 5.7
=> 5
ceiling 5.7
=> 6
abs (-5)
=> 5
min 6 7
=> 6
max 6 7
=> 7
sin (pi/2)
=> 1.0
sum [1..5]
=> 15
not True
=> False
True && False
=> False
-}

-- Exercício 2--
dobro x = x * 2

-- Exercício 3--
quad x = 2 * (dobro x)

-- Exercício 4--
hipo x y = sqrt((dobro x) + (dobro y))

--Exercicio 5--
dist a b c d = sqrt((dobro c - a) + (dobro d - b))

