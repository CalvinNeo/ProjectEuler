import Data.List (find)

nthTraingle :: Int -> Int
nthTraingle n = (1 + n) * n `div` 2

factorCount x = length (filter (\d -> x `mod` d == 0) [1 .. x])

-- x = 2n(2n+1)/2
-- n and n+1 are relatively prime
-- solve(x) = solve(n) * solve(2n+1)


main = print $ nthTraingle $ head $ filter (\x -> (solve x) >= 500 ) [1..]
  where 
    solve x | odd x = (factorCount x) * (factorCount ((x + 1) `div` 2))
    solve x | even x = (factorCount (x `div` 2)) * (factorCount (x + 1))  
