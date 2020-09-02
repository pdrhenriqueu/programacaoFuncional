-- Exercicio 3

Prelude> 1:[2,3,4]
[1,2,3,4]
Prelude> 'a':['b','c','d']
"abcd"
Prelude>
Prelude> head [1,2,3]
1
Prelude> tail [1,2,3]
[2,3]
Prelude> [1,5,2,3]!!1
5
Prelude> [1,5,2,3]!!3
3
Prelude> take 2 [1,5,2,3,7]
[1,5]
Prelude> drop 2 [1,5,2,3,7]
[2,3,7]
Prelude> [1,2] ++ [3,4]
[1,2,3,4]
Prelude> [1..10]
[1,2,3,4,5,6,7,8,9,10]
Prelude> [7,6..3]
[7,6,5,4,3]
Prelude> ['b'..'g']
"bcdefg"
Prelude> take 5 [1,3..]
[1,3,5,7,9]
Prelude> sum [1..10]
55
Prelude> maximum [1,5,2,3,7]
7
Prelude> minimum [1,5,2,3,7]
1