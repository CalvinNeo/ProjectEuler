import Data.List (find)

nthTraingle n = (1 + n) * n `div` 2

numDivisor n = length (filter (\x -> n `mod` x == 0) [1..n])

main = print $ find (\x -> (numDivisor x) >= 500 ) [1..]
