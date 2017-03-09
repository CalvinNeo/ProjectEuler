fib 1 = 1
fib 2 = 2
fib n = fib(n - 1) + fib(n - 2)

suitable fibx | fibx <= 400 && (even fibx) = True
              | otherwise = False

main = print $ sum $ (filter even (takeWhile (<4000000) [(fib x) | x <- [1..]])) 

-- 4613732