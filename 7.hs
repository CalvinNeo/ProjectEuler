naiveTest :: Int -> Bool
naiveTest n = all (\x -> n `mod` x /= 0) (takeWhile (\x -> x * x <= n) [2..])

nthPrime :: Int -> Int -> Int
nthPrime n s | n == 0 = s - 1
             | (naiveTest s) = nthPrime (n - 1) (s + 1)
             | otherwise = nthPrime n (s + 1)

main = print $ nthPrime 10001 2