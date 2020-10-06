--Trabalho 2

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
x7 = [20,8,2,11,13,3,7,18,14,4,16,10,15,1,9,17,19,12,5,6]

--Exercicio 1 
--A	
selectionsort :: Ord a => [a] -> [a]
selectionsort [] = []
selectionsort xs = [x] ++ selectionsort (remove x xs)
  where
    x = foldr1 min xs

remove :: Eq t => t -> [t] -> [t]
remove a [] = []
remove a (x : xs)
  | a == x = xs
  | otherwise = x : (remove a xs)
  
--B
insercao1:: (Ord a) => [a] -> [a]
insercao1 [] = []
insercao1 (x:xs) = foldr insereOrd1 [x] (insercao1 xs)

insereOrd1 :: Ord a => a -> [a] -> [a]
insereOrd1 x [] = [x] 
insereOrd1 x (y:ys)
 |x <= y = (x:y:ys)
 |otherwise = y: (insereOrd1 x ys)
 
 --C
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (s:xs) = quicksort (filter (< s) xs) ++ [s] ++ quicksort (filter (>= s) xs)

--2
--Variação 1

bolha2 :: [Int] -> [Int]
bolha2 [] = []
bolha2 l = bolhaOrd2 l (length l)
 
bolhaOrd2 :: [Int] -> Int -> [Int]
bolhaOrd2 l 0 = l
bolhaOrd2 l n = bolhaOrd2 (troca2 l (n-1)) (n-1)
 
troca2 :: [Int] -> Int -> [Int] 
troca2 [x] _ = [x]
troca2 l 0 = l
troca2 (x:y:r) n
 | x > y = y:troca2 (x:r) (n-1)
 | otherwise = x: troca2 (y:r) (n-1)
 