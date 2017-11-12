
digitSum = sum . map ((\x -> read x :: Int) . return) . show

main = print $ digitSum (2 ^ 1000)