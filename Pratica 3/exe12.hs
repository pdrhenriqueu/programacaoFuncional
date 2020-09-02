mdcG :: (Int,Int) -> Int
mdcG (m,n)
	|n == 0 = m
	|otherwise = mdcG (n, (mod m n))

mdcCP :: (Int,Int) -> Int
mdcCP (m,0) = m
mdcCP (m,n) = mdcCP (n, (mod m n))