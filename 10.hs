naiveTest :: Int -> Bool
naiveTest n = all (\x -> n `mod` x /= 0) (takeWhile (\x -> x * x <= n) [2..])

sumPrimeLT n = sum $ takeWhile (<= n) [x | x <- [2..], (naiveTest x)]

main = print $ sumPrimeLT 2000000

-- wait and result is 142913828922