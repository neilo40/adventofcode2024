import Text.Regex.TDFA

main :: IO()
main = do
    ls <- readLines "app/day13.testinput"
    let grouped = groupLines ls
    --Button A: X+94, Y+34 Button B: X+22, Y+67 Prize: X=8400, Y=5400 
    let pat = "Button A: X.([0-9]+), Y.([0-9]+) Button B: X.([0-9]+), Y.([0-9]+) Prize: X=([0-9]+), Y=([0-9]+).*"
    let machines = map (inputNumbers pat) grouped
    print machines
    print (rangesForMachine (head machines))

--data Machine = Machine (Int,Int) (Int,Int) (Int,Int) deriving (Show)

-- read all lines from file into list
readLines :: FilePath -> IO [String]
readLines fp = lines <$> readFile fp 

-- input list -> output, groups of 4 lines
groupLines :: [String] -> [String]
groupLines [] = []
groupLines ls = unwords (take 4 ls) : groupLines (drop 4 ls)

-- regex -> input string -> matched nums
inputNumbers :: String -> String -> [Int]
inputNumbers pat content = map read (fourth matches) where
    matches = content =~ pat :: (String,String,String,[String]) -- 4th element in tuple is all submatches

-- get the 4th value from the 4-tuple returned by the regex match
fourth :: (String, String, String, [String]) -> [String]
fourth (_, _, _, x) = x

-- looking for aA + bB = z, where a is the number of A presses, A is the increment, b is number of B presses, B is increment and z is sum (for x and y axes)
-- a and b are integers from 0 to z/A and 0 to z/B
findRange :: Int -> Int -> [Int]
findRange inc target = [0..target `div` inc]

-- list a -> list b -> all combinations of both lists
combinations :: [Int] -> [Int] -> [(Int,Int)]
combinations as bs = [(a,b) | a <- as, b <- bs]

-- machine -> combinations
rangesForMachine :: [Int] -> [(Int,Int)]
rangesForMachine m = combinations a b where
    a = findRange (head m) (m!!4)
    b = findRange (m!!2) (m!!4)