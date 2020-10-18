-- x[n+1] = x[n] + x[n-1]
fibs = 0:1:(zipAdd fibs (tail fibs))

zipAdd (x:xs) (y:ys) = (x+y):zipAdd xs ys
