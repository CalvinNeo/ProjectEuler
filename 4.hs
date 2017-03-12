import Data.List (sort, reverse)
isPara :: Int -> Bool
isPara x | (show x) == (reverse (show x)) = True
         | otherwise = False
main = print $ head $ reverse $ sort $ (filter (isPara) [x * y | x <- [100 .. 999], y <- [100 .. 999]] )
-- main = print (filter isPara [x * y | x <- [100 .. 999], y <- [100 .. 999]] )
