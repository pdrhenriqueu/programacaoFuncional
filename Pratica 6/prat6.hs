--Exercicio 1
paridade::[Int]->[Bool]
paridade x = map even x

--Exercicio 2
prefixos::[String]->[String]
prefixos x = map (take 3) x

--Exercicio 3
saudacao :: [String]->[String]
saudacao x = map ("Oi " ++) x

--Exercicio 4
filtar :: (a->Bool)->[a]-> [a]
filtar f [] = []
filtar f (x:xs) 
 |f x = x: filtar f xs
 |otherwise = filtar f xs
 
filtarComp:: (a->Bool)->[a]-> [a]
filtarComp f xs = [x | x<-xs, f x]

--Exericio 5

pares::[Int]->[Int]
pares xs = filter even xs

--Exercicio 6
solucoes::(Ord a, Num a)=>[a]->[a]
solucoes x = filter (\x->(5 * x + 6) < (x*x)) x

--Exercicio 7
maior::(Num a, Ord a)=>[a]->a
maior x = foldr1 max x

--Exercicio 8
menor_min10::[Int]->Int 	
menor_min10 x = foldr (min) 10 x

--Exercicio 9
junta_silabas_plural::[[Char]]->[Char]
junta_silabas_plural x =  foldr (++) "s" x

--Exercicio 10 
lst1 = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
lst2 = [20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1]
lst3 = [11,12,13,14,15,16,17,18,19,20,1,2,3,4,5,6,7,8,9,10]
lst4 = [10,9,8,7,6,5,4,3,2,1,20,19,18,17,16,15,14,13,12,11]
lst5 = [11,12,13,14,15,5,4,3,2,1,16,17,18,19,20,10,9,8,7,6]
lst6 = [1,12,3,14,5,15,4,13,2,11,6,17,8,19,20,10,9,18,7,16]
lst7 = [1..1000]
lst8 = [1000,999..1]
lst9 = lst1++[0]
lst10 = [0]++lst3
lst11 = lst1++[0]++lst3
lst12 = lst3++[0]++lst1
--bubblesort
bubblesort :: Ord a => [a] -> [a]
bubblesort lst = bubblesortordena lst (length lst)

bubblesortordena :: Ord a => [a] -> Int -> [a]
bubblesortordena lst 0 = lst
bubblesortordena lst t = bubblesortordena (troca lst) (t-1)

troca :: Ord a => [a] -> [a]
troca [x] = [x]
troca (x:y:xs)
 | x > y = y : troca (x:xs)
 | otherwise = x : troca(y:xs)

--selectionsort
selectionsort :: Ord a => [a] -> [a]
selectionsort [] = []
selectionsort xs = [x] ++ selectionsort (remove x xs)
  where
    x = minimum xs

remove :: Eq t => t -> [t] -> [t]
remove a [] = []
remove a (x : xs)
  | a == x = xs
  | otherwise = x : (remove a xs)
  
--insertionsort
insertionsort :: (Ord a) => [a] -> [a]
insertionsort [] = []
insertionsort (x : xs) = insereOrd x (insertionsort xs)

insereOrd :: Ord a => a -> [a] -> [a]
insereOrd x [] = [x]
insereOrd x (y : ys)
  | x <= y = (x : y : ys)
  | otherwise = y : (insereOrd x ys)

--quicksort
quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (s : xs) = quickSort [x | x <- xs, x < s] ++ [s] ++ quickSort [x | x <- xs, x >= s]



--11
--bubblesort

myBubblesort2 :: (Ord a) => [a] -> ([a], Int)
myBubblesort2 [] = ([], 0)
myBubblesort2 lista = bolhaOrd2 (lista, 0) (length lista)

bolhaOrd2 :: (Ord a, Num t, Num b, Eq t) => ([a], b) -> t -> ([a], b)
bolhaOrd2 (lista, count) 0 = (lista, count)
bolhaOrd2 (lista, count) n = bolhaOrd2 (troca2 (lista, count)) (n -1)

troca2 :: (Ord a, Num b) => ([a], b) -> ([a], b)
troca2 ([x], cont) = ([x], cont)
troca2 ((x : y : zs), cont) =
  if x > y
    then add (troca2 ((x : zs), cont + 1)) y
    else add (troca2 ((y : zs), cont + 1)) x
  where
    add (lista, count) a = (a : lista, count)

--selectionshort

mySelectionsort2 :: Ord a => [a] -> ([a], Int)
mySelectionsort2 lista = selectionAUX lista 0

selectionAUX :: (Ord a) => [a] -> Int -> ([a], Int)
selectionAUX [] n = ([], n)
selectionAUX (x : xs) n =
  let (least, n_num) = minimo2 (x : xs) n

      remove2 _ [] = []
      remove2 n (h : t) =
        if (n == h)
          then t
          else h : (remove2 n t)

      add (lst, n) y = (y : lst, n)
   in add (selectionAUX (remove2 least (x : xs)) n_num) least

minimo2 :: (Ord a) => [a] -> Int -> (a, Int)
minimo2 [] _ = undefined
minimo2 [x] cont = (x, cont)
minimo2 (x : y : xs) cont
  | x > y = minimo2 (y : xs) (cont + 1)
  | otherwise = minimo2 (x : xs) (cont + 1)

--insertionshort

myInsertionsort2 :: (Ord a) => [a] -> ([a], Int)
myInsertionsort2 [] = ([], 0)
myInsertionsort2 [x] = ([x], 0)
myInsertionsort2 (h : t) =
  let (sorted_tail, n) = myInsertionsort2 t

      (lst, n1) = insereOrd2 h sorted_tail n
   in (lst, n1)

insereOrd2 :: (Ord a) => a -> [a] -> Int -> ([a], Int)
insereOrd2 x [] n = ([x], n)
insereOrd2 x (h : t) n =
  if (x <= h)
    then ((x : h : t), n + 1)
    else add (insereOrd2 x t (n + 1)) h
  where
    add (list, n) y = (y : list, n)

--Quick
quickAux :: [a] -> Int -> (a -> Bool) -> ([a], Int)
quickAux [] n _ = ([], n)
quickAux (x : xs) n cond =
  if (cond x)
    then add (quickAux xs (n + 1) cond) x
    else quickAux xs (n + 1) cond
  where
    add (list, n) y = (y : list, n)

myQuicksort2 :: (Ord a) => [a] -> ([a], Int)
myQuicksort2 [] = ([], 0)
myQuicksort2 (piv : xs) =
  let (left, n_L) = quickAux xs 0 (<= piv)
      (right, n_R) = quickAux xs 0 (> piv)
      (sorted_L, n1_L) = myQuicksort2 left
      (sorted_R, n1_R) = myQuicksort2 right
   in (sorted_L ++ [piv] ++ sorted_R, n_L + n_R + n1_L + n1_R)

--12
--bubblesort
bubble_sort2 :: (Ord a) => [a] -> ([a], Int)
bubble_sort2 [] = ([], 0)
bubble_sort2 lista = bubble2 (lista, 0) (length lista)

bubble2 :: (Ord a, Num t, Num b, Eq t) => ([a], b) -> t -> ([a], b)
bubble2 (lista, auxcont) 0 = (lista, auxcont)
bubble2 (lista, auxcont) n = bubble2 (auxiliar2 (lista, auxcont)) (n -1)

auxiliar2 :: (Ord a, Num b) => ([a], b) -> ([a], b)
auxiliar2 ([x], cont) = ([x], cont)
auxiliar2 ((x : y : zs), cont) =
  if x > y
    then add (auxiliar2 ((x : zs), cont + 1)) y
    else add (auxiliar2 ((y : zs), cont + 1)) x
  where
    add (lista, auxcont) a = (a : lista, auxcont)

--selectionsort
selection_sort2 :: Ord a => [a] -> ([a], Int)
selection_sort2 lista = auxiliarSelection lista 0

auxiliarSelection :: (Ord a) => [a] -> Int -> ([a], Int)
auxiliarSelection [] n = ([], n)
auxiliarSelection (x : xs) n =
  let (least, n_num) = minimo2 (x : xs) n

      remove2 _ [] = []
      remove2 n (h : t) =
        if (n == h)
          then t
          else h : (remove2 n t)

      add (lst, n) y = (y : lst, n)
   in add (auxiliarSelection (remove2 least (x : xs)) n_num) least

minimo2 :: (Ord a) => [a] -> Int -> (a, Int)
minimo2 [] _ = undefined
minimo2 [x] cont = (x, cont)
minimo2 (x : y : xs) cont
  | x > y = minimo2 (y : xs) (cont + 1)
  | otherwise = minimo2 (x : xs) (cont + 1)

--insertionsort
insertion_sort2 :: (Ord a) => [a] -> ([a], Int)
insertion_sort2 [] = ([], 0)
insertion_sort2 [x] = ([x], 0)
insertion_sort2 (h : t) =
  let (sorted_tail, n) = insertion_sort2 t

      (lst, n1) = insere_ordenado2 h sorted_tail n
   in (lst, n1)

insere_ordenado2 :: (Ord a) => a -> [a] -> Int -> ([a], Int)
insere_ordenado2 x [] n = ([x], n)
insere_ordenado2 x (h : t) n =
  if (x <= h)
    then ((x : h : t), n + 1)
    else add (insere_ordenado2 x t (n + 1)) h
  where
    add (list, n) y = (y : list, n)
--quicksort
auxiliarQuick :: [a] -> Int -> (a -> Bool) -> ([a], Int)
auxiliarQuick [] n _ = ([], n)
auxiliarQuick (x : xs) n cond =
  if (cond x)
    then add (auxiliarQuick xs (n + 1) cond) x
    else auxiliarQuick xs (n + 1) cond
  where
    add (list, n) y = (y : list, n)

quicksort2 :: (Ord a) => [a] -> ([a], Int)
quicksort2 [] = ([], 0)
quicksort2 (piv : xs) =
  let (left, n_L) = auxiliarQuick xs 0 (<= piv)
      (right, n_R) = auxiliarQuick xs 0 (> piv)
      (sorted_L, n1_L) = quicksort2 left
      (sorted_R, n1_R) = quicksort2 right
   in (sorted_L ++ [piv] ++ sorted_R, n_L + n_R + n1_L + n1_R)	