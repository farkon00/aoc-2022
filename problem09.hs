import System.IO
import qualified Data.Set as Set

type Move = Char
type Coords = (Integer, Integer)
type State = ((Coords, Coords), Set.Set Coords)

getTailMove :: Coords -> Coords -> Coords
getTailMove (hx, hy) (tx, ty) =
    if abs (hx - tx) <= 1 && abs (hy - ty) <= 1 then
        (0, 0)
    else
        (
            if hx == tx then
                0
            else
                if hx > tx then
                    1
                else
                    -1,
            if hy == ty then
                0
            else
                if hy > ty then
                    1
                else
                    -1
        )

moveTail :: State -> State
moveTail ((head, tail@(tx, ty)), visited) = 
    let 
        (dx, dy) = getTailMove head tail
        newTail = (tx+dx, ty+dy)
    in
        ((head, newTail), Set.insert newTail visited)

moveByCharacter :: Move -> Coords
moveByCharacter 'L' = (-1, 0)
moveByCharacter 'R' = (1, 0)
moveByCharacter 'U' = (0, 1)
moveByCharacter 'D' = (0, -1)

moveHead :: Coords -> Move -> Coords
moveHead (hx, hy) move = 
    let (dx, dy) = moveByCharacter move in (hx+dx, hy+dy)

performMove :: State -> Move -> State
performMove ((head, tail), visited) move = moveTail ((moveHead head move, tail), visited)

type BigState = ([Coords], Set.Set Coords)

performMoveInChain :: Set.Set Coords -> BigState -> Coords -> BigState
performMoveInChain originalVisited (states, _) tail =
    let
        ((_, newTail), visited) = moveTail ((last states, tail), originalVisited)
    in
        (states ++ [newTail], visited)

performMoves :: BigState -> Move -> BigState
performMoves (states, visited) move = 
    foldl (performMoveInChain visited) ([moveHead (head states) move], visited) $ tail states

problem9_1 :: [Move] -> Integer
problem9_1 = toInteger . length . Set.toList . snd . foldl performMove (((0, 0), (0, 0)), Set.empty)

problem9_2 :: [Move] -> Integer
problem9_2 = toInteger . length . Set.toList . snd . foldl performMoves (replicate 10 (0, 0), Set.empty)

parseMove :: String -> [Move]
parseMove str = 
    let
        dir = head str
        distance = (read $ tail $ tail str) :: Integer
    in
        replicate (fromInteger distance) dir   

main = do
    handle <- openFile "input/input9.txt" ReadMode
    content <- hGetContents handle

    let 
        moves = foldl (\a b -> a ++ parseMove b) [] $ lines content
        in do
            print $ problem9_1 moves
            print $ problem9_2 moves

    hClose handle