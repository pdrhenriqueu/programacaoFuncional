resto_div::Int->Int->Int
resto_div m n
	|m == 0 = 0
	|m < n = m
	|otherwise = resto_div(m - n) n
	
div_inteira::Int->Int->Int
div_inteira m n
	|m < n = 0
	|otherwise = (div_inteira(m - n)n)+1