import Data.List.Split
import Data.Char (digitToInt)
import Debug.Trace

-- main = readFile "18.dat" >>= print . (map concat) . (splitOn " ") . lines

readTriangle :: String -> [[Int]]
readTriangle = \s -> (map ( (map (\y -> read y :: Int)) . words) (lines s))

-- trace ("i is " ++ show i ++ " len is " ++ show len ++ " Triangle i is " ++ show choice2)

-- solve :: [[Int]] -> Int -> Int -> [Int]
-- solve triangle len i = zipWith max choice1 choice2
--   where 
--   dp = if i >= (len - 1) then (triangle !! i) else solve triangle len (i+1)
--   choice1 = zipWith (+) (triangle !! i) dp 
--   choice2 = if i == 0 then choice1 else zipWith (+) (triangle !! i) (tail dp)

solve :: [[Int]] -> Int -> [Int]
solve triangle i 
  | i == (length triangle) - 1 = (triangle !! i)
  | otherwise = zipWith max choice1 choice2
      where 
      choice1 = zipWith (+) (triangle !! i) dp 
      choice2 = zipWith (+) (triangle !! i) (tail dp)
      dp = solve triangle (i+1)

solve3 :: [[Int]] -> Int
solve3 triangle = (solve triangle 0) !! 0

main = readFile "18.dat" >>= print . solve3 . readTriangle

