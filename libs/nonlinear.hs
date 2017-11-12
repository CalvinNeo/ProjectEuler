derive :: (Fractional a) => (a -> a) -> (a -> a)
derive f = f' where f' x = (f (x + dh) - f x) / dh
                    dh = 1e-6


newton :: (Fractional a, Ord a) => (a -> a) -> (a -> a) -> a -> a
newton f f' x | (abs . f) x < eps = x
              | otherwise = newton f f' (x - delta)
                  where eps = 1e-4
                        delta = (f x) / (f' x)

steffensen :: (Fractional a, Ord a, Show a) => (a -> a) -> a -> a
steffensen f x 
  | abs (f x - x) < 1e-6 = x
  | otherwise = steffensen f newx 
    where
      y = f x
      z = f y
      newx = x - (((y - x) * (y - x)) / (z - 2 * y + x)) 

f1 x = (2.0 - (exp x) + x * x) / 3.0

main = print $ steffensen f1 0.0