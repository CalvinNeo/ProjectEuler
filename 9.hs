main = print $ [ a * b * c | a <- [0..1000], b <- [0..1000 - a], let c = 1000 - a - b, a^2 + b^2 == c^2]
