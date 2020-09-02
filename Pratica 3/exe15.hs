criaLista::Int->Int->[Int]
criaLista a b
	|a == b = [a]
	|a > b = []
	|otherwise = [a..b]
	
listaPar::Int->Int->[Int]
listaPar a b 
 	|mod a 2 == 0 && mod b 2 == 0 = [a..b]
	|mod a 2 == 1 = [a+1..b]
	|mod b 2 == 1 = [a..b-1]
	|otherwise = []