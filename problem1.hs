import Inp
import Data.List (sort)

problem1_1 :: [[Integer]] -> Integer
problem1_1 = maximum . map sum

firstN :: Integer -> [a] -> [a]
firstN _ [] = []
firstN _ [x] = [x]
firstN 1 (x:xs) = [x]
firstN n (x:xs) = x:firstN (n - 1) xs

problem1_2 :: [[Integer]] -> Integer
problem1_2 = sum . firstN 3 . reverse . sort . map sum