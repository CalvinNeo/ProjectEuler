

diag :: (Num a) => [[a]] -> [a]
diag mat = zipWith (!!) mat [0..]

matrix :: (Num a) => Int -> Int -> ((Int, Int) -> a) -> [[a]]
matrix n m f = [[f (i,j) | j <- [0..m-1]] | i <- [0..n-1]]

diagMat :: (Num a) => [a] -> [[a]]
diagMat xs = matrix n n (\(i,j) -> if i == j then (xs!!i) else 0)
  where n = length xs

strictUpperMat :: (Num a) => [[a]] -> Int -> [[a]]
strictUpperMat mat n = matrix n n (\(i,j) -> if i < j then (mat!!i!!j) else 0)

strictLowerMat :: (Num a) => [[a]] -> Int -> [[a]]
strictLowerMat mat n = matrix n n (\(i,j) -> if i > j then (mat!!i!!j) else 0)

transpose :: (Num a) => [[a]] -> [[a]]
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)

row :: (Num a) => [[a]] -> Int -> [a]
row mat r = mat !! r

col :: (Num a) => [[a]] -> Int -> [a]
col mat c = zipWith (\xs r -> (xs !! c)) mat [0..]

rows :: (Num a) => [[a]] -> [[a]]
rows mat = mat

cols :: (Num a) => [[a]] -> [[a]]
cols mat = transpose mat

vecMul :: (Num a) => [a] -> [a] -> a
vecMul xs ys = sum $ zipWith (*) xs ys

matMul :: (Num a) => [[a]] -> [[a]] -> [[a]]
matMul mat1 mat2 = [[vecMul r c | c <- cols mat2] | r <- mat1]

matSub :: (Num a) => [[a]] -> [[a]] -> [[a]]
matSub mat1 mat2 = zipWith (\l1 l2 -> zipWith (-) l1 l2) mat1 mat2 

matAdd :: (Num a) => [[a]] -> [[a]] -> [[a]]
matAdd mat1 mat2 = zipWith (\l1 l2 -> zipWith (+) l1 l2) mat1 mat2 

matNeg :: (Num a) => [[a]] -> [[a]]
matNeg mat = [[0-c | c <- r] | r <- mat]

jacobi :: (Fractional a, Ord a) => [[a]] -> [[a]] -> [[a]] -> a -> [[a]]
jacobi a b x eps
  | abs (((a `matMul` x) `matSub` b) !! 0 !! 0) < eps = x
  | otherwise = jacobi a b newx eps
    where
      newx = (d' `matMul` ((d `matSub` a) `matMul` x) ) `matAdd` (d' `matMul` b) 
      d = diagMat $ diag a
      d' = diagMat $ (map (1.0/) (diag a))

seidel :: (Fractional a, Ord a) => [[a]] -> [[a]] -> [[a]] -> [[a]] -> a -> [[a]]
seidel a b x inv eps 
  | abs ((matSub (matMul a x) b) !! 0 !! 0) < eps = x
  | otherwise = seidel a b newx inv eps
    where 
      newx = (inv `matMul` b) `matSub` (bs `matMul` x)
      bs = inv `matMul` (strictUpperMat a 4)

main = print $ seidel a b x inv 1e-5
  where
    a = [[8.3, 2.1, -1.2, 0.5], [0.8, 10.2, 3.5, -1.8], [1.2, 0.2, -4.0, -0.5], [-0.2, 0.3, 0.4, -2]] 
    b = [[-3.02], [4.79], [-6.72], [8.89]] 
    x = [[1.0], [1.0], [1.0], [1.0]] 
    inv = [[0.120482, 0, 0, 0], [-0.00944956, 0.0980392, 0, 0], [0.0356721,0.00490196, -0.25, 0], [-0.00633121, 0.0156863, -0.05, -0.5]]
