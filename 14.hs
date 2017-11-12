collatz = (map collatz' [1..] !!)
  where
  collatz' x | odd x = 2 * x + 1
  collatz' x | even x = x `div` 2

main = print $ collatz 2