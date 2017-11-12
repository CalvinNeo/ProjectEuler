
main = print $ solve 20
  where solve n = fac (2 * n) `div` ((double . fac) n)
        double x = x * x
        fac 0 = 1
        fac x = x * fac (x - 1)