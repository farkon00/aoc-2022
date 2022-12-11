import System.IO
import Data.Text (Text, splitOn, pack, unpack)
import Data.List (sort)

data Operation = Addition Integer | Multiplication Integer | Square deriving Show

type Item = Integer
--                Divisiable by  If true  If false
type Algorithm = (Integer,      Integer, Integer)
--                                            Times
type Monkey = ([Item], Operation, Algorithm, Integer)

remove :: Eq a => [a] -> a -> [a]
remove (y:rest) x
    | x == y = rest
    | otherwise = y:remove rest x

replace :: (Eq a, Num a, Enum a) => a -> b -> [b] -> [b]
replace index elem = zipWith (\index' elem' -> (if index' == index then elem else elem')) [0..]

executeOper :: Operation -> Item -> Item
executeOper (Addition val) item = item + val
executeOper (Multiplication val) item = item * val
executeOper Square item = item ^ 2 

checkAlgorithm :: Algorithm -> Item -> Bool
checkAlgorithm (divisor, _, _) item = item `mod` divisor == 0

moveItem :: [Monkey] -> Integer -> Item -> Item -> Integer -> [Monkey]
moveItem monkeys index item newItem dstIndex = 
    let 
        (items, oper, algorithm, times) = monkeys !! fromInteger index
        (dstItems, dstOper, dstAlgorithm, dstTimes) = monkeys !! fromInteger dstIndex 
    in
        replace dstIndex (newItem:dstItems, dstOper, dstAlgorithm, dstTimes) $ 
            replace index (remove items item, oper, algorithm, times + 1) monkeys

executeAlgorithm :: (Integer -> Integer) -> Integer -> [Monkey] -> Item -> [Monkey]
executeAlgorithm calm index monkeys item =
    let
        monkey@(items, oper, algorithm@(_, onTrue, onFalse), times) = monkeys !! fromInteger index
        newItem = calm $ executeOper oper item
    in
        if checkAlgorithm algorithm newItem then
            moveItem monkeys index item newItem onTrue
        else
            moveItem monkeys index item newItem onFalse
        

executeMonkey :: (Integer -> Integer) -> [Monkey] -> Integer -> [Monkey]
executeMonkey calm monkeys index = 
    let
        (items, oper, algorithm, times) = monkeys !! fromInteger index    
    in
        foldl (executeAlgorithm calm index) monkeys items

updateMonkeys :: (Integer -> Integer) -> Integer -> [Monkey] -> Integer -> [Monkey]
updateMonkeys calm commonMod monkeys index
    | index >= toInteger (length monkeys) = monkeys
    | otherwise = 
        map (\(items, oper, algorithm, times) -> 
                (map (`mod` commonMod) items, oper, algorithm, times)) $
            updateMonkeys calm commonMod
                (executeMonkey calm monkeys index)
                (index + 1)

solve :: Integer -> (Integer -> Integer) -> [Monkey] -> Integer
solve gens calm monkeys = 
    let
        commonMod = product $ map (\(_, _, (x, _, _), _) -> x) monkeys
        monkeyResults = 
            reverse $ sort $ map (\(_, _, _, times) -> times) 
                (iterate (\monkeys -> updateMonkeys calm commonMod monkeys 0) monkeys !! fromInteger gens)
    in
        head monkeyResults * (monkeyResults !! 1)

parseMonkey :: String -> Monkey
parseMonkey str =
    let 
        -- Don't ask
        [_, 
            ' ':' ':'S':'t':'a':'r':'t':'i':'n':'g':' ':'i':'t':'e':'m':'s':':':' ':items, 
            ' ':' ':'O':'p':'e':'r':'a':'t':'i':'o':'n':':':' ':'n':'e':'w':' ':'=':' ':'o':'l':'d':' ':oper, 
            ' ':' ':'T':'e':'s':'t':':':' ':'d':'i':'v':'i':'s':'i':'b':'l':'e':' ':'b':'y':' ':test, 
            ' ':' ':' ':' ':'I':'f':' ':'t':'r':'u':'e':':':' ':'t':'h':'r':'o':'w':
            ' ':'t':'o':' ':'m':'o':'n':'k':'e':'y':' ':onTrue, 
            ' ':' ':' ':' ':'I':'f':' ':'f':'a':'l':'s':'e':':':' ':'t':'h':'r':'o':'w':
            ' ':'t':'o':' ':'m':'o':'n':'k':'e':'y':' ':onFalse] = lines str
    in
        (
            map ((read . unpack) :: (Text -> Integer)) $ splitOn (pack ", ") (pack items),
            if head oper == '*' then
                if tail (tail oper) == "old" then
                    Square
                else
                    Multiplication $ read $ tail $ tail oper  
            else
                Addition $ read $ tail $ tail oper,
            (read test, read onTrue, read onFalse),
            0
        )

main = do
    handle <- openFile "input/input11.txt" ReadMode
    content <- hGetContents handle

    let 
        monkeys = map (parseMonkey . unpack) $ splitOn (pack "\n\n") (pack content)
        in do
            print $ solve 20 (`div` 3) monkeys
            print $ solve 10000 id monkeys

    hClose handle