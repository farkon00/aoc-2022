import System.IO

--            Cycles     X
type State = ([Integer], Integer)

data Instruction = Noop | AddX Integer  

executeInstr :: State -> Instruction -> State
executeInstr (cycles, x) instr =
    case instr of
        Noop -> ([maximum cycles + 1], x)
        AddX val -> ([maximum cycles + 1, maximum cycles + 2], x + val)

isStrength :: Integer -> Bool
isStrength cycle = ((cycle - 20) `mod` 40) == 0

problem10_1 :: [Instruction] -> Integer
problem10_1 instrs =
    let 
        states = scanl executeInstr ([0], 1) instrs
        zippedStates = zip states [0..toInteger (length states)]
    in
        sum $ map (\((cycles, _), index) -> snd (states !! fromInteger (index-1)) * head (filter isStrength cycles)) $ 
            filter (any isStrength . fst . fst) zippedStates

getStateChar :: Integer -> Integer -> Char
getStateChar cycle x
    | abs ((cycle `mod` 40) - (x + 1)) <= 1 = '#'
    | otherwise = ' '

drawState :: String -> [State] -> (State, Integer) -> String
drawState prev states ((cycles, _), index) = prev ++ 
    concatMap
        (\cycle -> (
            if (cycle `mod` 40) == 0 then 
                ['\n', getStateChar cycle (snd (states !! fromInteger (if index == 0 then 0 else index - 1)))] 
            else 
                [getStateChar cycle (snd (states !! fromInteger (if index == 0 then 0 else index - 1)))])) 
            cycles

drawStates :: [State] -> String
drawStates states =
    let 
        zippedStates = zip states [0..toInteger (length states)]
    in
        foldl (`drawState` states) "" zippedStates


problem10_2 :: [Instruction] -> String
problem10_2 = drawStates . scanl executeInstr ([0], 1)


parseInstr :: String -> Instruction
parseInstr "noop" = Noop
parseInstr ('a':'d':'d':'x':' ':number) = AddX (read number :: Integer)

main = do
    handle <- openFile "input/input10.txt" ReadMode
    content <- hGetContents handle

    let 
        instrs = map parseInstr $ lines content
        in do
            print $ problem10_1 instrs
            putStrLn $ problem10_2 instrs

    hClose handle