-- Exercício 1 
dobro::Float->Float
dobro x = x * 2

quad::Float->Float
quad x = 2 * (dobro x)

hipo::Float->Float->Float
hipo x y = sqrt((dobro x) + (dobro y))

dist::Float->Float->Float->Float->Float
dist a b c d = sqrt((dobro c - a) + (dobro d - b))

{-
Exercício 2 
fst (2,5) => 2
snd (5, "Bom dia") => "Bom dia"
(1,1) == (1,1) => True
(1,1) /= (1,1) => False
(1,1) < (1,2) => True
(2,1) < (1,2) => False
(1,2,3) < (1,2) => Erro
"azul" < "verde" => True
"azul" < "amarelo" => False
(1,2,3) == (,,) 1 2 3 => True
-}
-- Exercício 3 
type Conversao = (Float, Float, Float)

conversao::Float->Conversao
conversao x = (x, (x * 4.45), (x * 3.96))

-- Exercício 4 

bissexto::Int->Bool

bissexto x 
    | mod x  4 == 0 && mod x 100 /= 0  = True
    | mod x 4 /= 0 && mod x 400 /= 0 = False
    | mod x 4 /= 0 && mod x 400 == 0 = True
    |otherwise = False

-- Exercício 5 
type Data = (Int,Int,Int)
bissexto2::Data->Bool

bissexto2 (d,m,a)
  | d >= 1 && d <= 31 && (m == 1 || m == 3 || m == 5 || m == 7 || m== 8 || m == 10 || m == 12) = True
  | d >= 1 && d <= 30 && (m == 4 || m == 6 || m == 9 || m == 11) = True
  | d >= 1 && d <= 28 && m == 2 && not (bissexto a) = True
  | d >= 1 && d <= 29 && m == 2 && (bissexto a) = True
  | otherwise = False

 -- Exercício 6 
valida::Data->Bool
valida (d,m,a)
   |bissexto a == True && m == 2 && d >= 1 && d <= 29 = True
   |bissexto a == False && m == 2 && d >= 1 && d <= 28 = True
   |d >= 1 && d <= 30 && m == 4 || m == 6 || m == 9 || m == 11  = True
   |d >= 1 && d <= 31 && m == 1 || m == 3 || m == 5 || m == 7 || m == 8 || m == 10 || m == 12 = True
   |otherwise = False

-- Exercício 7
precede::Data->Data->Bool
precede (d,m,a) (d1,m1,a1)
    | d <= d1 && m <= m1 && a <= a1 = True
    | m <= m1 && a <= a1 = True
    | a <= a1 = True
    |otherwise = False

-- Exercício 8
type Livros = (String, String, String, String, Int)
type Aluno = (String, String, String, Int)
type Emprestimo = (String, String, Data, Data, String)

-- Exercício 9
e1::Emprestimo
e1 = ("H123C9","BSI200945",(12,9,2009),(20,9,2009),"aberto")

emprestimo::Emprestimo->Data->Bool
emprestimo (_, _, _,x, _) y 
 | precede y x ==False = False 
 | otherwise = True
