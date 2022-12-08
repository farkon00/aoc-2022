import System.IO
import Data.Set (fromList, toList)

type Grid = [[Integer]]
type Coords = (Integer, Integer)

isVisible :: Grid -> Coords -> Bool
isVisible grid (x, y) = any (all (< ((grid !! fromInteger y) !! fromInteger x)))
    [
        take (fromInteger x) (grid !! fromInteger y),
        reverse $ take (length grid - fromInteger x - 1) $ reverse (grid !! fromInteger y),
        map (!! fromInteger x) (take (fromInteger y) grid),
        map (!! fromInteger x) (reverse $ take (length grid - fromInteger y - 1) $ reverse grid)
    ]

--getDirScenicScore :: Integer -> [Integer] -> Integer
getDirScenicScore origin xs = 
    let 
        res = toInteger $ length $ takeWhile (< origin) xs
    in
        if res /= toInteger (length xs) then
            res + 1
        else
            res
scenicScore :: Grid -> Coords -> Integer
scenicScore grid (x, y) = product $ map (getDirScenicScore ((grid !! fromInteger y) !! fromInteger x))
    [
        reverse $ take (fromInteger x) (grid !! fromInteger y),
        reverse $ take (length grid - fromInteger x - 1) $ reverse (grid !! fromInteger y),
        reverse $ map (!! fromInteger x) (take (fromInteger y) grid),
        map (!! fromInteger x) (reverse $ take (length grid - fromInteger y - 1) $ reverse grid)
    ]

problem8_1 :: Grid -> Integer
problem8_1 grid = toInteger $ length $ filter (isVisible grid)
    [(toInteger x, toInteger y) | x <- [0..length (head grid) - 1], y <- [0..length grid - 1]]

problem8_2 :: Grid -> Integer
problem8_2 grid = maximum $ [scenicScore grid (toInteger x, toInteger y) |
    x <- [0..length (head grid) - 1], y <- [0..length grid - 1]]

main = do
    handle <- openFile "input/input8.txt" ReadMode
    content <- hGetContents handle

    let 
        grid = map (map (\c -> (read [c] :: Integer))) $ lines content
        in do
            print $ problem8_1 grid
            print $ problem8_2 grid

    hClose handle