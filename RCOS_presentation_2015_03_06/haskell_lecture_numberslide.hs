factorial n = product [1..n]

dotProduct xs ys = sum $ zipWith (*) xs ys
magnitude xs = sqrt $ dotProduct xs xs

divides n i = (n `mod` i) == 0
isPrime n = not $ any (divides n) [2..n-1]

fibonaccis = 1 : 1 : zipWith (+) fibonaccis (tail fibonaccis)

approxDerivative epsilon f x = (f (x+epsilon) - f x) / epsilon

stars n = replicate (floor n) '*'
showLines = putStr . unlines
prepend = zipWith ((++) . show)
plot f xs = showLines $ prepend xs (map (stars . f) xs)
