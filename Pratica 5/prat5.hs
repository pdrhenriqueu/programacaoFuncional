--Exercicio 1A
bissexto::Int->Bool
bissexto x 
 |teste1 == 0 = True
 |(teste2 == 0) && (teste3 /= 0)  = True
 |otherwise = False
 where
    teste1 = (mod x 400)
    teste2 = (mod x  4 )
    teste3 = (mod x 100)


valida::Data->Bool
valida (d,m,a)
   |(bissexto a == True && teste4 == True) = True
   |(bissexto a == False && teste5 == True) = True
   |(teste6 == True) = True
   |(teste7 == True)  = True
   |otherwise = False
   where
   teste4 = ((bissexto a == True) && m == 2 && d >= 1 && d <= 29 )==True
   teste5 = (m == 2 && d >= 1 && d <= 28 )== True
   teste6 = (d >= 1 && d <= 30 && m == 4 || m == 6 || m == 9 || m == 11)== True
   teste7 = (d >= 1 && d <= 31 && m == 1 || m == 3 || m == 5 || m == 7 || m == 8 || m == 10 || m == 12)== True

--Exercicio 1B

bissextos :: [Int]-> [Int]
bissextos b = y
    where
      y = [x |x<-b,bissexto x]
--Exercicio 1C

type Data = (Int, Int, Int)
type Emprestimo = (String, String, Data, Data, String)
type Emprestimos = [Emprestimo]

bissexto2::Data->Bool

bissexto2 (d,m,a)
  | a1 == True = True
  | a2 == True = True
  | a3 == True = True
  | a4 == True = True
  | otherwise = False
  where
     a1 = d >= 1 && d <= 31 && (m == 1 || m == 3 || m == 5 || m == 7 || m== 8 || m == 10 || m == 12) == True
     a2 = d >= 1 && d <= 30 && (m == 4 || m == 6 || m == 9 || m == 11) == True
     a3 = (d >= 1 && d <= 28 && m == 2 && not (bissexto a)) == True
     a4 = (d >= 1 && d <= 29 && m == 2 && (bissexto a)) == True

  
bdEmprestimo :: Emprestimos
bdEmprestimo = a1
 where
 a1 = [ ("H123C9", "BSI945", (12, 9, 2009), (20, 09, 2009), "aberto"),
    ("L433C5", "BCC021", (01, 9, 2009), (10, 09, 2009), "encerrado"),
    ("M654C3", "BCC008", (04, 9, 2009), (15, 09, 2009), "aberto")
  ]


procede :: Data -> Data -> Bool
procede (dia, mes, ano) (dia2, mes2, ano2)
  |a1 = not (valida (dia, mes, ano)) || not (valida (dia2, mes2, ano2)) 
  |a2 = ano > ano2 
  |a3 = ano == ano2 && mes > mes2 
  |a4 = ano == ano2 && mes == mes && dia > dia2 
  |otherwise = True
  where
   a1 = False
   a2 = False
   a3 = False
   a4 = False

emprestimoEmDia :: Data -> Emprestimo -> Bool
emprestimoEmDia dataAtual (codigoLivro, codigoAluno, dataEmprestimo, dataDevolucao, status) =
  procede dataAtual dataDevolucao

atrasados :: Emprestimos -> Data -> Emprestimos
atrasados listaEmprestimos dataAtual = a1
 where
 a1 = [x | x <- listaEmprestimos, not (emprestimoEmDia dataAtual x)]

--Exercicio 1D
passo :: (Int, Int) -> (Int, Int)
passo (x, y) = a1
  where
    a1 = (y, x + y)

fibo2 :: Int -> (Int, Int)
fibo2 0 = (0, 1)
fibo2 n = a1
  where
    a1 = passo (fibo2 (n -1))

-- Exercicio 1E 
prodIntervalo :: Int -> Int -> Int
prodIntervalo m n = if(m > n) then a1 else a2
 where
 a1 = 1
 a2 = m * prodIntervalo(m+1) n

fatorial::Int->Int
fatorial n1 = z
              where
              z = prodIntervalo 1 n1




--Exercicio 2A
bissextoLet::Int->Bool
bissextoLet x = let
                 teste1 = (mod x 400 == 0)
                 teste2 = (mod x  4 == 0 )
                 teste3 = (mod x 100 /= 0)
                in teste1 || (teste2 && teste3)

validaLet::Data->Bool
validaLet (d,m,a) = let
                   teste4 = ((bissextoLet a == True) && m == 2 && d >= 1 && d <= 29 )
                   teste5 = (m == 2 && d >= 1 && d <= 28 )
                   teste6 = (d >= 1 && d <= 30 && m == 4 || m == 6 || m == 9 || m == 11)
                   teste7 = (d >= 1 && d <= 31 && m == 1 || m == 3 || m == 5 || m == 7 || m == 8 || m == 10 || m == 12)
                   in bissextoLet a && teste4 || bissextoLet a && teste5 || teste6 || teste7 

--Exercicio 2B
bissextosLet :: [Int]->[Int]
bissextosLet b = let
                y = [x |x<-b,bissexto x]
                 in y
--Exercicio 2C
bissexto2Let::Data->Bool

bissexto2Let (d,m,a) = let
                      a1 = d >= 1 && d <= 31 && (m == 1 || m == 3 || m == 5 || m == 7 || m== 8 || m == 10 || m == 12)
                      a2 = d >= 1 && d <= 30 && (m == 4 || m == 6 || m == 9 || m == 11)
                      a3 = (d >= 1 && d <= 28 && m == 2 && not (bissextoLet a))
                      a4 = (d >= 1 && d <= 29 && m == 2 && (bissextoLet a))
                     in a1 || a2 || a3 || a4

     
bdEmprestimoLet :: Emprestimos
bdEmprestimoLet = let 
                 x =[ ("H123C9", "BSI945", (12, 9, 2009), (20, 09, 2009), "aberto"),
                      ("L433C5", "BCC021", (01, 9, 2009), (10, 09, 2009), "encerrado"),
                      ("M654C3", "BCC008", (04, 9, 2009), (15, 09, 2009), "aberto")]
                in x

procedeLet :: Data -> Data -> Bool
procedeLet (dia, mes, ano) (dia2, mes2, ano2) = let
                                                 a1 = not (validaLet (dia, mes, ano)) || not (validaLet (dia2, mes2, ano2)) 
                                                 a2 = ano > ano2 
                                                 a3 = ano == ano2 && mes > mes2 
                                                 a4 = ano == ano2 && mes == mes && dia > dia2 
                                                in a1 || a2 || a3 || a4
emprestimoEmDiaLet :: Data -> Emprestimo -> Bool
emprestimoEmDiaLet dataAtual (codigoLivro, codigoAluno, dataEmprestimo, dataDevolucao, status) =
  procedeLet dataAtual dataDevolucao

atrasadosLet :: Emprestimos -> Data -> Emprestimos
atrasadosLet listaEmprestimos dataAtual = let 
                                           y =[x | x <- listaEmprestimos, not (emprestimoEmDia dataAtual x)]
                                          in y
--Exercicio 2D
passoLet :: (Int, Int) -> (Int, Int)
passoLet (x, y) = let
                a1 = (y, x + y)
               in a1

fibo2Let :: Int -> (Int, Int)
fibo2Let 0 = (0, 1)
fibo2Let n = let
               a1 = passoLet(fibo2Let(n-1))
             in a1
--Exercicio 2E
prodIntervaloLet :: Int -> Int -> Int
prodIntervaloLet m n = let
						 a1 = if (m >= n) then n else (m * (prodIntervaloLet (m + 1) n))
                       in a1 

fatInterLet :: Int -> Int
fatInterLet n = let
			    a1 = prodIntervalo 1 n
                in a1
				
--Exercicio 3
--1 (λx. 2*x + 1) 3 = 2*3+1 = 6+1 = 7

--2 (λxy. x-y) 5 7 = 5-7 = -2

--3 (λyx. x-y) 5 7 = 7-5 = 2

--4 (λxy. x-y) (λz. z/2) = (λxy. x-y) z/2 = z/2-z/2 = 0

--5 (λxy. x-y) ((λz. z/2)6) 1 = (λxy. x-y) (6/2)1 = (λxy. x-y) 3 1 = 3 - 1 = 2

--6 (λ x. λ y. – x y ) 9 4 =  9 - 4 = 5

--Exercicio 4
-- (\x -> x + 3) 5
--8

--(\x -> \y -> x * y + 5) 3 4
--17

--(\(x,y) -> x * y^2) (3,4)
--48

--(\(x,y,_) -> x * y^2) (3,4,2)
--48

--(\xs -> zip xs [1,2,3]) [4,5,6]
--[(4,1),(5,2),(6,3)]

--Exercicio 5 
--a) (λx λy. y)((λz. z)(λz. z))(λw. w) 5 = (\x-> \y-> y)((\z-> z)(\z-> z))(\w-> w) 5 = 5

--b) ((λf. (λx. f(f x))) (λy. (y * y))) 3 = ((λf. (λx. (λy. (y * y))((λy. (y * y)) x))) (λy. (y * y))) 3
--((\f-> (\x-> (\y-> (y * y))((\y-> (y * y)) x))) (\y-> (y * y))) 3 = 81

--c) ((λf. (λx. f(f x)))(λy.(+ y y))) 5 = ((\f-> (\x->f(f x)))(\y->(y+y))) 5 = 20

--d) ((λx. (λy. + x y) 5) ((λy. - y 3) 7)) = ((\x-> (\y->x+y) 5) ((\y->y-3) 7)) = 9

--e) (((λf. (λx. f(f(f x)))) (λy. (y * y))) 2) =  (((\f->(\x->f(f(f x)))) (\y->(y * y))) 2) = 256

--f) (λx. λy. + x ((λx. - x 3) y)) 5 6 =  (\x-> \y->x+((\x->x-3) y)) 5 6 = 8