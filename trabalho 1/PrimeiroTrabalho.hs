--1
triangulo::(Int,Int,Int)->String
triangulo (x,y,z)
 |(x + y + z) > 180 = "nao_triangulo"
 |(x == y) && (y == z) && (x == z) = "equilatero"
 |x == 90 || y == 90 || z == 90 = "retangulo"
 |x > 90 || y > 90 || z > 90 = "obtuso"
 |otherwise = "simples"
 
--2
equacao::Float->Float->Float->(Float, Float)
equacao a b c
 |a /= 0 = (((-b)+sqrt((b^2)-(4*(a*c)))) , ((-b)-sqrt((b^2)-(4*(a*c)))))
 |otherwise = ((-c / b) ,(a))

--3
idade::Data->Data->Int
idade (d,m,a) (d1,m1,a1)
 |d < d1 && m < m1 = (a1-a)+1
 |d > d1 && m < m1 = (a1-a)+1
 |d > d1 && m > m1 = a1-a
 |otherwise = a1-a
 
type Data = (Int,Int,Int)
passagem::Float->Data->Data->Float
passagem p (d,m,a) (d1,m1,a1) 
 |idade (d,m,a) (d1,m1,a1) < 2 =  p-(p*0.15)
 |idade (d,m,a) (d1,m1,a1) <= 10 = p-(p*0.4)
 |idade (d,m,a) (d1,m1,a1) >= 70 = p-(p*0.5)
 |otherwise = p
 
--4

--a
a = [x^2 | x <-[4..14], odd x]
--[25,49,81,121,169]

--b
b = [(x, x*2) | x<-[1..4], x>1 && x<4]
--[(2,4),(3,6)]

--c
c = [ y | x<-[10..15], y<-[1..x]]
--[1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,11,1,2,3,4,5,6,7,8,9,10,11,12,1,2,3,4,5,6,7,8,9,10,11,12,13,1,2,3,4,5,6,7,8,9,10,11,12,13,14,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]
--d
d = [(x,y) | x<-[1..16], y<-[1..16], odd x, even y, x == y-1]
--[(1,2),(3,4),(5,6),(7,8),(9,10),(11,12),(13,14),(15,16)]
--e
e = [(x+y) | x<-[1..16], y<-[1..16], odd x, even y, x == y-1]
--[3,7,11,15,19,23,27,31]

--5
--A
contaNegM2 a = length[x | x<-a, x < 0 && mod x 2 == 0]

--B
listaNegM2 a = [x | x<-a, x < 0 && mod x 2 == 0]

--6
distancias::[(Float,Float)]->[Float]
distancias a = [sqrt((x^2) + (y^2)) | (x,y)<-a] 

--7 
fatores::Int->[Int]
fatores a = [x | x<-[1..a], (mod a x)==0]
ehprimo::Int->Bool
ehprimo x = if fatores (x)==[1,x] then True else False

primos::Int->Int->[Int]
primos a b = [x | x<-[a..b], ehprimo (x) == True]

--8
mdc::(Int,Int) -> Int
mdc (x,y)
 |y == 0 = x 					-- caso base quando n(divisor) == 0, o dividendo(m) e o mdc
 |otherwise = mdc (y, (mod x y)) -- se o divisor não for 0 divida o x y 


mmc2::Int->Int->Int
mmc2 n1 n2 =  div (n1*n2) (mdc (n1,n2))

mmc::Int->Int->Int->Int
mmc n1 n2 n3 = mmc2 n1 (mmc2 n2 n3)

--9
calculaSerie :: Int -> Int -> Float
calculaSerie x 1 = fromIntegral(div 1 x)
calculaSerie x n = if (mod n 2 == 0) then fromIntegral(div n x) + calculaSerie x(n-1) 
 else fromIntegral (div x n) + calculaSerie x (n-1)
 
--10
fizzbuzz :: Int -> [String]
fizzbuzz 0 = [] -- caso base (n == 0)
fizzbuzz n
    | (mod n 3 == 0) && (mod n 5 == 0) = fizzbuzz(n-1) ++ ["FizzBuzz"] -- ++ concatena o conteudo na string
    | (mod n 3 == 0) = fizzbuzz(n-1) ++ ["Fizz"]
    | (mod n 5 == 0) = fizzbuzz(n-1) ++ ["Buzz"]
    | otherwise = fizzbuzz(n-1) ++ ["No"]

--11
conta_ocorrencias1::Int->[Int]->Int
conta_ocorrencias1 n [] = 0
conta_ocorrencias1 n (x:xs)
 |n==x = 1+(conta_ocorrencias1 n xs)  
 |otherwise = conta_ocorrencias1 n xs


conta_ocorrencias::Int->Int->[Int]->(Int, Int) 
conta_ocorrencias n m l1 = (conta_ocorrencias1 n l1, conta_ocorrencias1 m l1) 

--12
unica_ocorrencia::Int->[Int]->Bool
unica_ocorrencia n l1
 |(conta_ocorrencias1 n l1) == 0 = False
 |(conta_ocorrencias1 n l1) > 1 = False
 |otherwise = True
 
--13
intercala::[Int]->[Int]->[Int]
intercala [] (y:ys) = (y:ys)
intercala (x:xs) [] = (x:xs)
intercala (x:xs) (y:ys) = x:intercala (y:ys) xs

--14
type Contato = (String, String, Int, String) 
contatos :: [Contato] 
contatos = 
  [ ("Fulano", "Rua 1", 123, "fulano@gmail.com"),
    ("Fulano de", "Rua 2", 456, "fulanode@gmail.com"),
    ("Fulano de tal", "Rua 3", 789, "fulanodetal@gmail.com")]
	
aux :: [Contato] -> String -> String 
aux [] email = "Email nao encontrado"
aux ((nome,_,_, emailInserido) : x) email 
  | emailInserido == email = nome 
  | otherwise = aux x email 
  
encontraContato :: String -> String 
encontraContato emailEncontrar = aux contatos emailEncontrar  



--15
type Pessoa = (String, Float, Int, Char)
pessoas :: [Pessoa]
pessoas = [ ("Rosa", 1.66 ,27,'C'),
 ("Joao", 1.85, 26, 'C'),
 ("Maria", 1.55, 62, 'S'),
 ("Jose", 1.78, 42, 'C'),
 ("Paulo", 1.93, 25, 'S'),
 ("Clara", 1.70, 33, 'C'),
 ("Bob", 1.45, 21, 'C'),
 ("Rosana", 1.58,39, 'S'),
 ("Daniel", 1.74, 72, 'S'),
 ("Jocileide", 1.69, 18, 'S') ]
 
 --Fazer media das idades 
contPessoas::[Pessoa]->Float
contPessoas [] = 0
contPessoas (x:xs) = 1+contPessoas xs
 
somaMedia::[Pessoa]->Float
somaMedia [] = 0
somaMedia((_,altura,_,_):xs) = altura+somaMedia xs
 
alturaMedia::[Pessoa]->Float
alturaMedia [] = 0
alturaMedia l1 = somaMedia l1 / contPessoas l1


--Idade da pessoa mais nova
 
idadeMenor::[Pessoa]->Int
idadeMenor [(a,b,idade,c)] = idade
idadeMenor((a,b,idade,c):(a1,b1,idade1,c1):xs)
 |idade<idade1 = idadeMenor((a,b,idade,c):xs)
 |otherwise = idadeMenor((a1,b1,idade1,c1):xs)
 
--O nome e o estado civil da pessoa mais velha
nomeEstado::[Pessoa]->[Pessoa]
nomeEstado [(nome,b,idade,estado)] = [(nome,b,idade,estado)]

nomeEstado ((nome,b,idade,estado):(nome1,b1,idade1,estado1):xs)
 |idade>idade1 = nomeEstado((nome,b,idade,estado):xs)
 |otherwise = nomeEstado((nome1,b1,idade1,estado1):xs)

--Todos os dados de cada pessoa com 50 anos ou mais.
mais50::[Pessoa]->[Pessoa]
mais50 l = [(nome,altura,idade,sexo) | (nome,altura,idade,sexo)<-l, idade>=50]

--O número de pessoas casadas com idade superior a i (ex: i = 35).
numeroDePessoas::[Pessoa]->Int->Int
numeroDePessoas l1 n = length[(nome, altura, idade, sexo) | (nome, altura, idade, sexo)<-l1, sexo=='C', idade>n]

--16

insere_ord::Ord a => a -> [a] -> [a]
insere_ord n [] = [n]
insere_ord n (x:xs) 
 |n <= x = (n:x:xs)
 |otherwise = x:insere_ord n xs
 
--17
reverte::[a]->[a]
reverte [] = []
reverte (x:xs) = (reverte xs)++[x]

--18 
remove_repetidos:: Ord n => n -> [n] -> [n] 
remove_repetidos x [] = [] 
remove_repetidos x (y:ys) 
    |x == y = remove_repetidos x ys 
    |otherwise = y:(remove_repetidos x ys) 

sem_repetidos :: Ord n => [n] -> [n] 
sem_repetidos [] = [] 
sem_repetidos (x:xs) = x:(sem_repetidos (remove_repetidos x xs)) 

--19
disponiveis :: [Int]
disponiveis = [1,2,5,10,20,50,100]

notas_troco :: Int -> [[Int]]
notas_troco 0 = [[]]
notas_troco n = [ x:xs | x<-disponiveis, x <= n, xs<-notas_troco (n-x) ]
                