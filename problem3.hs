import System.IO
import Data.Char (isUpper, ord)
import Data.Set (toList, fromList, intersection)

type ItemType = Char
type Compartment = [ItemType] -- String
type Rucksuck = (Compartment, Compartment)

commonTypes :: Rucksuck -> [ItemType]
commonTypes rucksuck = toList $ intersection (fromList (fst rucksuck)) (fromList (snd rucksuck))

getPriority :: ItemType -> Integer
getPriority itemType = 
    if isUpper itemType then
        toInteger (ord itemType) - 38
    else
        toInteger (ord itemType) - 96

getPriorities :: [ItemType] -> Integer
getPriorities = sum . map getPriority

problem3_1 :: [Rucksuck] -> Integer
problem3_1 = sum . map (getPriorities . commonTypes)

type Elf = [ItemType]
type Group = (Elf, Elf, Elf)

getGroups :: [Rucksuck] -> [Group]
getGroups [first, second, third] = 
    [(
        uncurry (++) first,
        uncurry (++) second, 
        uncurry (++) third
    )]
getGroups rucksucks = head (getGroups (take 3 rucksucks)):
    getGroups (reverse $ take (length rucksucks - 3) $ reverse rucksucks)

getBadge :: Group -> ItemType
getBadge (first, second, third) = head $ commonTypes (commonTypes (first, second), third)

problem3_2 :: [Rucksuck] -> Integer
problem3_2 = sum . map (getPriority . getBadge) . getGroups

main = do
    handle <- openFile "input/input3.txt" ReadMode
    content <- hGetContents handle
    
    let
        rucksuck = map (\line -> splitAt (length line `div` 2) line) $ lines content
        in do
            print $ problem3_1 rucksuck
            print $ problem3_2 rucksuck
    
    hClose handle