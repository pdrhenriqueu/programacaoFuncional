--Exercicio 1
(||) ::Bool->Bool->Bool
False || False = False
True || True = True
False || True = True
True || False = True

(||) ::Bool->Bool->Bool
False || False = False
_|| _  = True

(||) ::Bool->Bool->Bool
False || b = b
True  || _ = True

ou::Bool->Bool->Bool
ou (x,y)
 |x == False || y == False = False
 |otherwise = True

ou (x,y) = if x == False || y == False then False
          else True
