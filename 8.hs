import Data.Char (ord)

removeCrlf :: String -> String
removeCrlf (x:xs) | x == '\n' = removeCrlf xs
                  | otherwise = [x] ++ (removeCrlf xs)
removeCrlf [] = []

parseInt :: String -> [Int]
parseInt [] = []
parseInt (x : xs) = [(ord x) - (ord '0')] ++ (parseInt xs)

repeatMultiply :: Int -> [Int] -> [Int]
repeatMultiply 0 xs = xs
repeatMultiply n xs = zipWith (*) xs (repeatMultiply (n - 1) (tail xs))

solve :: [Int] -> Int
solve s = maximum $ repeatMultiply 12 s

main = readFile "8.dat" >>= (return . solve . parseInt . removeCrlf) >>= print