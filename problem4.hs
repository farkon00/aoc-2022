import System.IO
import Data.List (elemIndex)

type Elf = (Integer, Integer)
type Pair = (Elf, Elf)

applyBothPairArientations :: (Pair -> Bool) -> Pair -> Bool
applyBothPairArientations f (first, second) = 
    f (first, second) || f (second, first)


fullyContains :: Pair -> Bool
fullyContains ((gf, ls), (lf, gs)) = (lf <= gf) && (ls <= gs) 

overlaps :: Pair -> Bool
overlaps ((f, _), (ls, hs)) = f <= hs && f >= ls

solve :: (Pair -> Bool) -> [Pair] -> Integer
solve f = toInteger . length . filter (applyBothPairArientations f)

parseElf :: String -> Elf
parseElf elf = let 
        (first, second) = splitAt (maybe 0 id $ elemIndex '-' elf) elf
    in
        (read first :: Integer, read (tail second) :: Integer)

main = do
    handle <- openFile "input/input4.txt" ReadMode
    content <- hGetContents handle
    
    let 
        pairs = map (\line -> splitAt (maybe 0 id $ elemIndex ',' line) line) (lines content)
        inp = map (\(f, s) -> (parseElf f, parseElf (tail s))) pairs
        in do
            print $ solve fullyContains inp
            print $ solve overlaps inp
    
    hClose handle