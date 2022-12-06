import System.IO
import Data.Set (toList, fromList)

isStartOfPacket :: Integer -> String -> Bool
isStartOfPacket len = ((fromInteger len) ==) . length . toList . fromList

solve :: Integer -> String -> Integer
solve len str = 
    if isStartOfPacket len (take (fromInteger len) str) then
        len
    else
        (toInteger 1) + solve len (tail str)
        
main = do
    handle <- openFile "input/input6.txt" ReadMode
    content <- hGetContents handle
    
    print $ solve 4 content
    print $ solve 14 content

    hClose handle