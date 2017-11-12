import Debug.Trace
import Data.Char
import Data.List


fac :: Integer -> Integer
fac n
  | n == 0 = 1 
  | otherwise = n * fac (n - 1)

digits :: Integer -> [Integer]
digits = reverse . unfoldr (\x -> if x == 0 then Nothing else Just (mod x 10 :: Integer, div x 10 :: Integer))

main = print $ sum $ digits $ fac 100