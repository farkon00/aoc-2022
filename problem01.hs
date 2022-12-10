import System.IO
import Data.List (sort)
import Data.Text (unpack, pack, splitOn)

problem1_1 :: [[Integer]] -> Integer
problem1_1 = maximum . map sum

problem1_2 :: [[Integer]] -> Integer
problem1_2 = sum . take 3 . reverse . sort . map sum

main = do
    handle <- openFile "input/input1.txt" ReadMode
    content <- hGetContents handle
    
    let 
        elfs = map (map (read :: (String -> Integer)) . lines . unpack) (splitOn (pack "\n\n") (pack content))
        in do
            print $ problem1_1 elfs
            print $ problem1_2 elfs
    
    hClose handle