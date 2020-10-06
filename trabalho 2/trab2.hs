--RENATA CRISTINA GOMES DA SILVA 11721BCC012--
--PEDRO HENRIQUE DA SILVA OLIVEIRA 11811BCC040--

l1=[1..1000]
l2=[1000,999..1]
l3=l1++[0]
l4=[0]++l2
l5=l1++[0]++l2
l6=l2++[0]++l1
l7=l2++[0]++l2
x1=[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
x2=[20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1]
x3=[11,12,13,14,15,16,17,18,19,20,1,2,3,4,5,6,7,8,9,10]
x4=[10,9,8,7,6,5,4,3,2,1,20,19,18,17,16,15,14,13,12,11]
x5=[11,12,13,14,15,5,4,3,2,1,16,17,18,19,20,10,9,8,7,6]
x6=[1,12,3,14,5,15,4,13,2,11,6,17,8,19,20,10,9,18,7,16]
x7=[20,8,2,11,13,3,7,18,14,4,16,10,15,1,9,17,19,12,5,6]

--PARTE A--
--1A SELECTION--
selection_sort::(Ord a)=>[a]->[a] 
selection_sort [] = [] 
selection_sort [x] = [x] 
selection_sort (x:xs) = 
    let auxiliar = foldr1 (min) (x:xs) 

        remover _ [] = [] 
        remover a (head:tail) = if a == head then tail else head : (remover a tail) 

    in [auxiliar] ++ selection_sort (remover auxiliar (x : xs)) 

--1B INSERTION --
insertion_sort::(Ord a)=>[a]->[a] 
insertion_sort y = foldr (insereOrdenado) [] y 
    where 
        insereOrdenado x [] = [x] 
        insereOrdenado x (head:tail) = if x <= head then (x:head:tail) else head : (insereOrdenado x tail) 

--1C QUICKSORT--
quick_sort::(Ord a) => [a] -> [a] 
quick_sort [] = [] 
quick_sort (pivo:xs) = (quick_sort (filter (pivo >) xs)) ++ [pivo] ++ (quick_sort (filter (pivo<=) xs)) 

--2--
-- Algoritmo original Bubble Sort--
bolha [] = []
bolha lista = bolhaOrd lista (length lista)
bolhaOrd lista 0 = lista
bolhaOrd lista n = bolhaOrd (trocaLista lista) (n-1)

troca [x] = [x]
troca (x:y:zs)
 | x>y = y:troca (x:zs)
 | otherwise = x:troca(y:zs)

--VARIAÇÃO 1--
variacao_bubble1::Ord t => [t]->[t] 
variacao_bubble1 [] = [] 
variacao_bubble1 lista = variacao_bubbleOrd1 lista (length lista) 

variacao_bubbleOrd1::Ord t => [t] -> Int -> [t] 
variacao_bubbleOrd1 lista 0 = lista 
variacao_bubbleOrd1 lista num = do 
    if contador /= 0 then 
        variacao_bubbleOrd1 lista(num-1) 
        else lista 
        where
            (lista,contador) = trocar(lista,0) 

trocar::Ord t => ([t], Int) -> ([t], Int) 
trocar ([x], num) = ([x], num) 
trocar ((x:y:zs), num) = if x > y then 
    auxiliar (trocar ((x:zs), num+1)) y else 
    auxiliar (trocar ((y:zs), num)) x 
     where 
         auxiliar (lista, num) t = (t:lista, num) 

--VARIAÇÃO 2--
variacao_bubble2 :: Ord a => [a] -> [a]
variacao_bubble2 [] = [] 
variacao_bubble2 lista = variacao_bubbleOrd2 lista (length lista) 

variacao_bubbleOrd2 :: Ord a => [a] -> Int -> [a]
variacao_bubbleOrd2 lista 0 = lista 
variacao_bubbleOrd2 lista n = variacao_bubbleOrd2 (troca2 lista) (n-1) 

ordenacao_bubble [x] = [x] 
ordenacao_bubble l = (ordenacao_bubble body) ++ last 
    where 
        lista = troca2 l 
        last = aux2 lista 
        body = aux1 lista 

aux1 :: [a] -> [a]
aux1 lista = take (length lista - 1) lista 

aux2 :: [a] -> [a]
aux2 lista = drop (length lista - 1) lista

troca2 :: Ord a => [a] -> [a]
troca2 [x] = [x]
troca2 (x:y:zs) = if x > y then 
  y:(troca2 (x:zs)) else 
  x:(troca2 (y:zs)) 

--VARIAÇÃO 3--
bubbleSort3 :: (Ord t) => [t] -> [t]
bubbleSort3 [] = [] 
bubbleSort3 a =
  let adiciona (a, b) y = (y : a, b) 
      auxiliar1 lista = (take (length lista - 1) lista, drop (length lista - 1) lista) 
      troca ([x], auxiliar) = ([x], auxiliar) 
      troca ((x : y : xs), auxiliar) = if x > y then adiciona (troca ((x : xs), 1)) y else adiciona (troca ((y : xs), auxiliar)) x 
      bubble ([x], auxiliar) = ([x], auxiliar) 
      bubble (lista, auxiliar)
        | n_auxiliar == 0 = (lista, auxiliar) 
        | otherwise = (fst (bubble (sera_trocado, 0)) ++ ultimo_elemento, 0) 
        where
          (lista_trocada, n_auxiliar) = troca (lista, auxiliar) 
          (sera_trocado, ultimo_elemento) = auxiliar1 lista_trocada 
   in fst (bubble (a, -1))

--VARIAÇÃO 1 CONTADOR--
bubble1Cont::[Int]->([Int],Int)
bubble1Cont [] = ([],0)
bubble1Cont l = bubble_ordena1Cont l 1

bubble_ordena1Cont :: [Int] -> Int ->([Int],Int)
bubble_ordena1Cont l 0 = (l,0)
bubble_ordena1Cont l a = (n,x1 + qtd)
 where
  (naux,number,qtd) = troca1Contador l
  (n,x1) = bubble_ordena1Cont naux number

troca1Contador::[Int]->([Int],Int,Int)
troca1Contador [x]= ([x],0,1)
troca1Contador(x:y:r) 
 | x > y = (x1,n1+1,n3+1)
 | otherwise = (x2,n2,n4+1)
 where
  (xaux,n1,n3) = troca1Contador(x:r)
  x1 = y:xaux
  (xaux2,n2,n4) = troca1Contador(y:r)
  x2 = x:xaux2

--VARIAÇÃO 2 CONTADOR--
bubble2Cont :: [Int] -> ([Int],Int)
bubble2Cont [] = ([],0)
bubble2Cont l = bubble_ordena2Cont l (length l)

bubble_ordena2Cont :: [Int] -> Int -> ([Int],Int)
bubble_ordena2Cont l 0 = (l,0)
bubble_ordena2Cont l n = (lista,naux + x1)
 where
  (aux,naux) = troca2Contador l (n-1)
  (lista,x1) = bubble_ordena2Cont aux (n-1)                

troca2Contador :: [Int] -> Int -> ([Int],Int) 
troca2Contador [x] _ = ([x],1)
troca2Contador l 0 = (l,1)
troca2Contador (x:y:r) n
   | x > y = (x1,1+n1)
   | otherwise =(x2,n2+1)
   where
    (xaux,n1) = troca2Contador(x:r) (n-1)
    x1 = y:xaux
    (xaux2,n2) = troca2Contador(y:r) (n-1)
    x2 = x:xaux2

--VARIAÇÃO 3 CONTADOR--
bubble3Cont :: [Int] -> ([Int],Int)
bubble3Cont [] = ([],0)
bubble3Cont l = bubble_ordena3Cont l (length l) 1

bubble_ordena3Cont :: [Int]-> Int -> Int ->([Int], Int)
bubble_ordena3Cont l _ 0 = (l,0)
bubble_ordena3Cont l n a = (n2,t1+t)
 where 
  (naux,number,t) = troca3Contador l (n-1)
  (n2,t1) = bubble_ordena3Cont naux (n-1) number

troca3Contador :: [Int] -> Int -> ([Int], Int,Int)
troca3Contador [x] _ = ([x],0,1)
troca3Contador l 0 = (l,0,1)
troca3Contador (x:y:r) n
 | x > y = (x1,n1+1,num1+1)
 | otherwise = (x2,n2,num2+1)
 where
  (xaux,n1,num1) = troca3Contador (x:r) (n-1)
  x1 = y:xaux
  (x2aux,n2,num2) = troca3Contador (y:r) (n-1)
  x2 = x:x2aux

---Conclusão: --

--Realizado os testes notei que a variação 2 demora mais, e as outras tem um tempo parecido de execução--

--Algoritmo orginal Selection Sort
selection_sortOriginal::(Ord a)=>[a]->[a]
selection_sortOriginal [] = []
selection_sortOriginal xs = x:selection_sortOriginal (remove x xs)
 where x = minimo1 xs

remove::(Ord a )=>a->[a]->[a]
remove a [] = []
remove a (x:xs)
 | a==x = xs
 | otherwise = x:(remove a xs)

minimo::(Ord a)=>[a]->a
minimo [] = undefined
minimo [x] = x
minimo (x:xs)
 | x<=(minimo xs) =  x
 | otherwise = minimo xs

--EXERCÍCIO 3--
--Variação 1--
selection_sort1::(Ord a)=>[a]->[a]
selection_sort1 [] = []
selection_sort1 xs = [x] ++selection_sort1 (remove x xs)
 where x = minimo xs

remove1::(Ord a )=>a->[a]->[a]
remove1 a [] = []
remove1 a (x:xs)
 | a==x = xs
 | otherwise = x:(remove1 a xs)

minimo1::(Ord a)=>[a]->a
minimo1 [] = undefined
minimo1 [x] = x
minimo1 (x:xs)
 | x<=(minimo1 xs) =  x
 | otherwise = minimo1 xs


--Variacao 2--
selection_sort2::[Int]->[Int]
selection_sort2 [] = []
selection_sort2 (x:xs) = y:selection_sort2 n
 where
 (n1,n2,y) = remove_menor xs x
 n = n1++n2

remove_menor::[Int] -> Int -> ([Int],[Int],Int)
remove_menor [] x = ([],[],x)
remove_menor (x:xs) n 
 | n < x = (n1,[x]++n2,c)
 | otherwise = ([n]++n3,n4,b)
 where
  (n1,n2,c) = remove_menor xs n
  (n3,n4,b) = remove_menor xs x

--Variação 2 com Contador--
selection_sort2Contador :: [Int]->([Int],Int)
selection_sort2Contador [] = ([],0)
selection_sort2Contador (h:t) = (x:teste,aux1+aux2)
 where
  (n1,n2,x,aux1) = remove_menor2Contador t h
  a = n1++n2
  (teste,aux2) = selection_sort2Contador a

remove_menor2Contador :: [Int] -> Int -> ([Int],[Int],Int,Int)
remove_menor2Contador [] x = ([],[],x,1)
remove_menor2Contador (h:t) x 
   | x < h = (n1,[h]++n2,c,aux1+1)
   | otherwise = ([x]++n3,n4,b,aux2+1)
   where
    (n1,n2,c,aux1) = remove_menor2Contador t x
    (n3,n4,b,aux2) = remove_menor2Contador t h

--Conclusões: 

--Neste caso selection_sort1  e selection_sortOriginal tem tempo similar de ordenação, mas o selection_sort2 demora mais comparado aos outros--


--EXERCÍCIO 4--

--Algoritmo Original QuickSort--
quick_sortOriginal::(Ord a )=>[a]->[a]
quick_sortOriginal [] = []
quick_sortOriginal (x:xs) = quick_sortOriginal [h | h<- xs, h<x] 
                   ++ [x] ++
                   quick_sortOriginal [h | h<- xs, h>=x]

--Variacao 1--
quick_sort1::(Ord a)=>[a]->[a]
quick_sort1 [] =  []
quick_sort1 (x:xs) = menores ++ [x] ++ maiores
 where 
 menores = filter (<x) xs
 maiores = filter (>x) xs

--Variação 2--
quick_sort2::Ord a=>[a]->[a]
quick_sort2 [] = []
quick_sort2 (x:xs) = quick_sort2 menores ++ [x] ++ quick_sort2 maiores
 where (menores,maiores) = divide2 x xs

divide2::Ord a =>a->[a]->([a],[a])
divide2 a [] = ([],[])
divide2 a (x:xs)
 | x < a = (x:x1,x2)
 | x >= a = (x1,x:x2)
 where (x1,x2) = divide2 a xs

-- Variacao 3--
quick_sort3 :: Ord a => [a]->[a]
quick_sort3 [] = []
quick_sort3 lista = quick_sort3 n  ++ [p] ++ quick_sort3 n2
 where
 (p,l1) = pivo3 lista
 (n,n2) = divide2 p l1

pivo3 :: Ord a => [a]->(a,[a])
pivo3 [x] = (x,[])
pivo3 [x,y] = (x,[y])
pivo3 (x:y:z:t) 
 | x >= y && x >= z = (x,(y:z:t))
 | y >= x && y >= z = (y,(x:z:t))
 | otherwise = (z,(x:y:t))


-- Variação 2 com contador--
quick_sort2Contador::Ord a=>[a]->([a],Int)
quick_sort2Contador [] = ([],0)
quick_sort2Contador (x:xs) = (l1 ++ [x] ++ l2,n+n1+n2)
 where
 (l1,n1) =  quick_sort2Contador menores
 (l2,n2) = quick_sort2Contador maiores
 (menores,maiores,n) = divide2Contador x xs

divide2Contador::Ord a =>a->[a]->([a],[a],Int)
divide2Contador a [] = ([],[],0)
divide2Contador a (h:t)
 | h < a = (h:h1,h2,n+1)
 | h >= a = (h1,h:h2,n+1)
 where 
 (h1,h2,n) = divide2Contador a t


--Variacao 3 com contador--
quick_sort3c :: Ord a => [a]->([a],Int)
quick_sort3c [] = ([],0)
quick_sort3c lista = (lista1,t1+t2+t3+t4)   
 where
 (p,l1,t1) = pivo3Contador lista
 (n,n2,t2) = divide2Contador p l1
 (auxiliar1 , t3) = quick_sort3c n
 (auxiliar2,t4) = quick_sort3c n2
 lista1 = auxiliar1 ++ [p] ++ auxiliar2

pivo3Contador :: Ord a => [a]->(a,[a],Int)
pivo3Contador [x] = (x,[],0)
pivo3Contador [x,y] = (x,[y],0)
pivo3Contador (x:y:z:t) 
 | x >= y && x >= z = (x,(y:z:t),2)
 | y >= x && y >= z = (y,(x:z:t),2)
 | otherwise = (z,(x:y:t),2)


--Conclusão: Neste caso o quick_sort se sai muito mais rápido quando comparado aos outros dois quick, o original e o da variacao 2

--EXERCÍCIO 5--
--Merge Sort--
merge_sort ::Ord a=>[a]->[a]
merge_sort [] = []
merge_sort [x] = [x]
merge_sort xs = merge (merge_sort ys) (merge_sort zs)
 where
 (ys,zs) = divide xs


divide ::Ord a=> [a] -> ([a],[a])
divide [] = ([],[])
divide [x] = ([x],[])
divide (x:y:t)  = ((x:xs),(y:ys))
            where
                (xs,ys) = divide t

merge :: Ord a=> [a]->[a]->[a]
merge xs [] = xs
merge [] xs = xs
merge (x:xs) (y:ys)
 | x < y = x:(merge xs (y:ys))
 | otherwise = y: (merge (x:xs) ys)


-- Merge sort com contador 
merge_sort2 ::Ord a=>[a]->([a],Int)
merge_sort2 [] = ([],0)
merge_sort2 [x] = ([x],1)
merge_sort2 xs = (e,b+d+f)
  where
  (ys,zs) = divide xs
  (a,b) = merge_sort2 ys
  (c,d) = merge_sort2 zs
  (e,f) = merge2 a c

merge2 :: Ord a=> [a]->[a]->([a],Int)
merge2 xs [] = (xs,1)
merge2 [] xs = (xs,1)
merge2 (x:xs) (y:ys)
 | x < y = (a,b+1)
 | otherwise = (c,d+1)
 where
 (a1,b) = merge2 xs (y:ys)
 a = x:a1
 (c1,d) = merge2 (x:xs) ys
 c = y:c1

-- Bucket Sort
auxiliarBucket :: Int -> Int -> Int -> Int -> [[Int]] -> [[Int]]
auxiliarBucket numero a b c [bucket] = if ((numero * a) `div` b) <= c then [numero : bucket] else [bucket]
auxiliarBucket numero a b c (bucket : buckets)
  | ((numero * a) `div` b) <= c = (numero : bucket) : buckets
  | otherwise = bucket : (auxiliarBucket numero a b (c + 1) buckets)

bucket_sort :: [Int] -> [Int]
bucket_sort [] = []
bucket_sort [x] = [x]
bucket_sort l1 =
  let a = length l1
      b = foldr1 (max) l1
      buckets = [[] | _ <- [1 .. a]]
      newBuckets = foldr (\x -> auxiliarBucket x a b 1) buckets l1
      sortedBuckets = map (merge_sort) newBuckets
      finalList = foldr1 (++) sortedBuckets
   in finalList

--Conclusão: o algoritmo mais rápido é o merge_sort por conta do tempo aparente para a resolução dos algoritmos.

--PARTE B--
--EXERCÍCIO 6--
data Exp a
  = Val a -- um numero
  | Add (Exp a) (Exp a) -- soma de duas expressoes
  | Sub (Exp a) (Exp a) --subtração
  | Mult (Exp a) (Exp a)
  | Pot (Exp a) (Exp a)

avalia :: Floating a => Exp a -> a
avalia (Val x) = x
avalia (Add exp1 exp2) = (avalia exp1) + (avalia exp2)
avalia (Sub exp1 exp2) = (avalia exp1) - (avalia exp2)
avalia (Mult exp1 exp2) = (avalia exp1) * (avalia exp2)
avalia (Pot exp1 exp2) = (avalia exp1) ** (avalia exp2)

expressao1 :: Exp Integer
expressao1 = (Mult (Add (Val 3) (Val 12)) (Pot (Sub (Val 15) (Val 5)) (Mult (Val 1) (Val 3))))

expressao2 :: Exp Integer
expressao2 = (Sub (Val 0) (Mult (Add (Add (Val 6) (Val 8)) (Sub (Val 1) (Val 5))) (Add (Val 2) (Pot (Val 6) (Val 2)))))

--EXERCÍCIO 7--

data Hora = PM Int Int
  | AM Int Int
  deriving (Eq, Show, Ord)

validaHora :: Int -> Bool
validaHora h
  | h > 0 && h <= 11 = True
  | otherwise = False

validaMinutos :: Int -> Bool
validaMinutos m
  | m >= 0 && m <= 59 = True
  | otherwise = False

horasDecorridas :: Hora -> Int
horasDecorridas (AM hora min)
  | validaHora (hora) == True && validaMinutos (min) == True = hora
  | otherwise = undefined
horasDecorridas (PM hora min)
  | validaHora (hora) == True && validaMinutos (min) == True = 12 + hora
  | otherwise = undefined

minutosDecorridos :: Hora -> Int
minutosDecorridos (AM hora min)
  | validaHora (hora) == True && validaMinutos (min) == True = hora * 60 + min
  | otherwise = undefined
minutosDecorridos (PM hora min)
  | validaHora (hora) == True && validaMinutos (min) == True = ((12 + hora) * 60) + min
  | otherwise = undefined

segundosDecorridos :: Hora -> Int
segundosDecorridos (AM hora min)
  | validaHora (hora) == True && validaMinutos (min) == True = (hora * 60 + min) * 60
  | otherwise = undefined
segundosDecorridos (PM hora min)
  | validaHora (hora) == True && validaMinutos (min) == True = (((12 + hora) * 60) + min) * 60
  | otherwise = undefined

--EXERCÍCIO 8--
type Data = (Int, Int, Int)
precendencia :: Data -> Data -> Bool
precendencia (d1, m1, y1) (d2, m2, y2)
  | y1 > y2 = False
  | y1 == y2 && m1 > m2 = False
  | y1 == y2 && m1 == m2 && d1 > d2 = False
  | otherwise = True

data Contato = Nome String | Telefone String
  deriving (Eq, Show)
data Mensagem = Msg Contato String Data Hora String
  deriving (Show)

-- A --
mensagens :: [Mensagem]
mensagens =
  [ (Msg (Nome "Fulaninho") "Mensagem 1" (16, 10, 1999) (AM 09 30) "WhatsApp"),
    (Msg (Telefone "123") "Mensagem 2" (16, 10, 1999) (AM 08 31) "WhatsApp"),
    (Msg (Nome "Fulaninha") "Mensagem 3" (16, 10, 1999) (AM 07 32) "Linkedin"),
    (Msg (Nome "Fulaninha") "Mensagem 4" (16, 10, 1999) (AM 12 33) "WhatsApp"),
    (Msg (Nome "Fulaninho") "Mensagem 5" (16, 10, 1999) (AM 11 37) "Facebook"),
    (Msg (Nome "Fulaninha") "Mensagem 6" (16, 10, 1999) (AM 3 30) "Facebook"),
    (Msg (Nome "Fulaninha") "Mensagem 7" (16, 10, 1999) (AM 4 35) "WhatsApp"),
    (Msg (Telefone "123") "Mensagem 8" (16, 10, 1999) (AM 11 37) "Linkedin"),
    (Msg (Nome "Fulaninho") "Mensagem 9" (16, 10, 1999) (AM 7 39) "Facebook"),
    (Msg (Nome "Fulaninho") "Mensagem 11" (16, 10, 1999) (AM 2 42) "Facebook"),
    (Msg (Nome "Fulaninha") "Mensagem 11" (16, 10, 1999) (AM 11 5) "WhatsApp"),
    (Msg (Telefone "123") "Mensagem 12" (16, 10, 1999) (AM 11 43) "Facebook"),
    (Msg (Nome "Fulaninho") "Mensagem 13" (16, 10, 1999) (AM 06 53) "WhatsApp"),
    (Msg (Nome "Fulaninha") "Mensagem 14" (16, 10, 1999) (AM 11 22) "Facebook"),
    (Msg (Nome "Fulaninha") "Mensagem 15" (16, 10, 1999) (AM 11 44) "WhatsApp"),
    (Msg (Nome "Fulaninho") "Mensagem 16" (19, 03, 2018) (PM 3 28) "WhatsApp"),
    (Msg (Nome "Fulaninho") "Mensagem 17" (19, 03, 2018) (PM 3 35) "Linkedin"),
    (Msg (Nome "Fulaninho") "Mensagem 18" (19, 03, 2018) (PM 3 24) "Facebook"),
    (Msg (Nome "Fulaninha") "Mensagem 19" (19, 03, 2018) (PM 3 57) "Facebook"),
    (Msg (Nome "Fulaninho") "Mensagem 20" (19, 03, 2018) (PM 3 35) "WhatsApp"),
    (Msg (Nome "Fulaninho") "Mensagem 21" (19, 03, 2018) (PM 3 33) "Facebook"),
    (Msg (Nome "Fulaninha") "Mensagem 22" (19, 03, 2018) (PM 3 48) "WhatsApp"),
    (Msg (Telefone "123") "Mensagem 23" (19, 03, 2018) (PM 4 50) "Linkedin"),
    (Msg (Nome "Fulaninho") "Mensagem 24" (19, 03, 2018) (PM 4 27) "WhatsApp"),
    (Msg (Nome "Fulaninha") "Mensagem 25" (19, 03, 2018) (PM 4 44) "Facebook"),
    (Msg (Nome "Fulaninho") "Mensagem 26" (19, 03, 2018) (PM 4 50) "WhatsApp"),
    (Msg (Telefone "123") "Mensagem 27" (19, 03, 2018) (PM 4 30) "Linkedin"),
    (Msg (Nome "Fulaninha") "Mensagem 28" (19, 03, 2018) (PM 4 40) "Facebook"),
    (Msg (Telefone "123") "Mensagem 29" (19, 03, 2018) (PM 4 30) "WhatsApp"),
    (Msg (Nome "Fulaninho") "Mensagem 30" (19, 03, 2018) (PM 4 20) "Linkedin")
  ]

-- B --
ordena_contato :: [Mensagem] -> [Mensagem]
ordena_contato [] = []
ordena_contato list = bubble list (length list)

bubble :: [Mensagem] -> Int -> [Mensagem]
bubble list 0 = list
bubble list n = bubble (trocaLista list) (n -1)

trocaLista :: [Mensagem] -> [Mensagem]
trocaLista [x] = [x]
trocaLista (mensagem1 : mensagem2 : xs)
  | comparacao mensagem1 mensagem2 = mensagem2 : trocaLista (mensagem1 : xs)
  | otherwise = mensagem1 : trocaLista (mensagem2 : xs)
  where
    comparacao (Msg (Nome _) _ _ _ _) (Msg (Telefone _) _ _ _ _) = True -- Ocorre troca, telefone vem primeiro
    comparacao (Msg (Telefone _) _ _ _ _) (Msg (Nome _) _ _ _ _) = False -- Não ocorre troca
    comparacao (Msg (Nome nome1) _ _ _ _) (Msg (Nome nome2) _ _ _ _) = nome1 > nome2
    comparacao (Msg (Telefone nome1) _ _ _ _) (Msg (Telefone nome2) _ _ _ _) = nome1 > nome2

-- C --
precede_mensagem :: Mensagem -> Mensagem -> Bool
precede_mensagem (Msg _ _ data1 hora1 _) (Msg _ _ data2 hora2 _)
  | data1 == data2 = (minutosDecorridos hora1) < (minutosDecorridos hora2)
  | otherwise = precendencia data1 data2

ordenaDataHora :: [Mensagem] -> [Mensagem]
ordenaDataHora [] = []
ordenaDataHora (piv : xs) =
  (ordenaDataHora [x | x <- xs, (precede_mensagem x piv) == False])
    ++ [piv]
    ++ (ordenaDataHora [x | x <- xs, (precede_mensagem x piv) == True])

-- D --
ultimas_mensagens :: Contato -> [Mensagem] -> [Mensagem]
ultimas_mensagens contact msgs = take 2 [(Msg c m d h a) | (Msg c m d h a) <- msgOrd, c == contact]
  where
    msgOrd = ordenaDataHora msgs

--EXERCÍCIO 9--
-- A --
data ArvoreBinInt = Nulo | No Int ArvoreBinInt ArvoreBinInt deriving Show

internos::ArvoreBinInt->[Int]
internos Nulo = []
internos (No  x Nulo Nulo) = []
internos (No x esquerda direita) = [x]++internos(esquerda)++internos(direita)

-- B --
soma_nos::ArvoreBinInt->Int
soma_nos Nulo = 0
soma_nos (No x Nulo Nulo) = x
soma_nos (No x esquerda direita) = x+soma_nos(esquerda)+soma_nos(direita)

-- C --
pertence::Int->ArvoreBinInt->Bool
pertence _ Nulo = False
pertence a (No x esquerda direita)
 | a==x = True
 | otherwise = (pertence a esquerda) || (pertence a direita) 

--EXERCÍCIO 10--

data ArvBinEA a = Vazia | 
                  Folha a | 
                  NoEA (Char, ArvBinEA a, ArvBinEA a) 
                  deriving (Show)

ea::ArvBinEA Float
ea = NoEA ('+', NoEA('*', Folha 10, Folha 5), Folha 7)


calcula :: Floating a => ArvBinEA a -> a
calcula Vazia = 0
calcula (Folha a) = a
calcula (NoEA (a,esq,dir)) = algebra a (calcula esq) (calcula dir)

algebra :: Floating a => Char -> a->a->a
algebra a b c 
 | a == '+' = b+c
 | a == '*' = b*c
 | a == '/' = b/c
 | a == '-' = b-c