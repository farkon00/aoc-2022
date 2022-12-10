import System.IO
import qualified Data.String as S

getResultScore :: (Char, Char) -> Integer
getResultScore ('A', 'X') = 3
getResultScore ('A', 'Y') = 6
getResultScore ('A', 'Z') = 0
getResultScore ('B', 'X') = 0
getResultScore ('B', 'Y') = 3
getResultScore ('B', 'Z') = 6
getResultScore ('C', 'X') = 6
getResultScore ('C', 'Y') = 0
getResultScore ('C', 'Z') = 3

getMoveScore :: Char -> Integer
getMoveScore 'X' = 1
getMoveScore 'Y' = 2
getMoveScore 'Z' = 3

getRoundScore :: (Char, Char) -> Integer
getRoundScore round = getResultScore round + getMoveScore (snd round)

problem2_1 :: [(Char, Char)] -> Integer
problem2_1 = sum . map getRoundScore

getResultScoreByLetter :: Char -> Integer
getResultScoreByLetter 'X' = 0
getResultScoreByLetter 'Y' = 3
getResultScoreByLetter 'Z' = 6

getMoveScore2 :: (Char, Char) -> Integer
getMoveScore2 ('A', 'X') = 3
getMoveScore2 ('B', 'X') = 1
getMoveScore2 ('C', 'X') = 2
getMoveScore2 ('A', 'Y') = 1
getMoveScore2 ('B', 'Y') = 2
getMoveScore2 ('C', 'Y') = 3
getMoveScore2 ('A', 'Z') = 2
getMoveScore2 ('B', 'Z') = 3
getMoveScore2 ('C', 'Z') = 1

getRoundScore2 :: (Char, Char) -> Integer
getRoundScore2 round = getResultScoreByLetter (snd round) + getMoveScore2 round

problem2_2 :: [(Char, Char)] -> Integer
problem2_2 = sum . map getRoundScore2

main = do
    handle <- openFile "input/input2.txt" ReadMode
    content <- hGetContents handle
    
    let 
        moves = map (\line -> (head line, last line)) (lines content)
        in do
            print $ problem2_1 moves
            print $ problem2_2 moves
    
    hClose handle