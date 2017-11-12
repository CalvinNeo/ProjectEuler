
slice :: Int -> Int -> ([a] -> [a])
slice f t = (drop f) . (take t)

main = readFile "13.dat" >>= print . slice 0 10 . show . sum . fmap (\x -> read x :: Integer) . lines