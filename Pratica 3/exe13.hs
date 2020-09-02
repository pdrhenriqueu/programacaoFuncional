binoGuardas :: (Int,Int) -> Int
binoGuardas (n,k)
	| k == 0 = 1
	| k == n = 1
	| otherwise = binoGuardas (n-1,k) + binoGuardas (n-1,k-1)
	
binomialCP :: (Int,Int) -> Int
binomialCP (n,k)
	|k == 0 = 1
	|k == n = 1
	|otherwise = binomialCP (n-1,k) + binomialCP (n-1,k-1)