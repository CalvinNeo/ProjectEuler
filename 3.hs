removeFactor n p | n `mod` p == 0 = (removeFactor (n `div` p) p)
                 | otherwise = n
-- takePrimeFactor n [x] = (removeFactor n x)
-- takePrimeFactor n (x:xs) = (takePrimeFactor (removeFactor n x) xs)
-- maxPrimeFactor n = (takePrimeFactor n (takeWhile (\x -> x * x < n) [2..]))
-- main = print $ (maxPrimeFactor 13195)

takePrimeFactor n p | p * p > n = n
                    | n `mod` p == 0 = max p (takePrimeFactor (removeFactor n p) (p + 1))
                    | otherwise = takePrimeFactor n (p + 1)

main = print $ (takePrimeFactor 600851475143  2)
