import Inp
import Data.List (sort)

problem1_1 :: [[Integer]] -> Integer
problem1_1 = maximum . map sum

problem1_2 :: [[Integer]] -> Integer
problem1_2 = sum . take 3 . reverse . sort . map sum