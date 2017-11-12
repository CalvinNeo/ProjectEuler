import Data.Char (ord)


parseInt :: String -> Int -> Int
parseInt [] n = n
parseInt (x:xs) n = parseInt xs (10 * n + (ord x)-(ord '0'))

-- read2DArr :: String -> [[Int]]
-- read2DArr s =  (takeWhile (/= ' ') (takeWhile (/= '\n') s))
-- read2DArr s = (fmap (parseInt . lines) s)

valid :: Int -> Int -> Bool
valid x y = (&&) (x `elem` [0 .. 19]) (y `elem` [0 .. 19])

cell :: Int -> Int -> [[Int]] -> Int
cell x y b 
  | not (valid x y) = 0
  | otherwise = (b !! x) !! y

chain :: [[Int]] -> (Int, Int) -> (Int, Int) -> Int -> Int
chain b p@(px,py) d@(dx,dy) 0 = cell px py b
chain b p@(px,py) d@(dx,dy) n = (cell px py b) * (chain b (px + dx, py + dy) d (n - 1))

solve :: [[Int]] -> Int
solve b = maximum [chain b (x,y) d 3 | x <- [0 .. 19], y <- [0 .. 19], d <- [(0, 1), (1, 0), (1, 1), (1, -1)] ]

blanks' :: Char -> [String] -> [String]
blanks' ' ' ss  = [] : ss   
blanks' c []  = [[c]]  
blanks' c (s : ss)  = (c : s) : ss

blanks :: String -> [String]
blanks = foldr blanks' []

main = do
  str <- readFile "11.dat" 
  let b = fmap (\x -> (fmap (\y -> parseInt y 0) x)) (fmap blanks (lines str))
  print $ solve b

