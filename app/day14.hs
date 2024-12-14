import Text.Regex.TDFA

rows :: Int
rows = 103 --7

cols::Int 
cols = 101 --11

iterations::Int
iterations = 100

-- position(row,col) vector(row,col)
data Robot = Robot (Int,Int) (Int,Int) deriving (Show)

main :: IO()
main = do
    ls <- readLines "app/day14.input"
    let robots = getAllRobots ls
    let endRobots = map (iterateBot iterations) robots
    let q1 = length [r | r <- endRobots, inQuadrant r 0 ((rows `div` 2)-1) 0 ((cols `div` 2)-1)]
    let q2 = length [r | r <- endRobots, inQuadrant r 0 ((rows `div` 2)-1) ((cols `div` 2)+1) (cols-1)]
    let q3 = length [r | r <- endRobots, inQuadrant r ((rows `div` 2)+1) (rows-1) 0 ((cols `div` 2)-1)]
    let q4 = length [r | r <- endRobots, inQuadrant r ((rows `div` 2)+1) (rows-1) ((cols `div` 2)+1) (cols-1)]
    print (product [q1,q2,q3,q4])

-- read all lines from file into list
readLines :: FilePath -> IO [String]
readLines fp = lines <$> readFile fp 

-- list of specs -> list of robots
getAllRobots :: [String] -> [Robot]
getAllRobots = map extractRobot

-- spec -> robot
extractRobot :: String -> Robot
extractRobot s = robotFromMatches (fourth (s =~ pat :: (String,String,String,[String]))) where
    -- p=0,4 v=3,-3
    pat = "p=([0-9]+),([0-9]+) v=(.*),(.*)"

-- spec has col,row.  we are used to row,col
robotFromMatches :: [String] -> Robot
robotFromMatches m = Robot (read (m!!1),read (head m)) (read (m!!3), read (m!!2))

-- get the 4th value from the 4-tuple returned by the regex match
fourth :: (String, String, String, [String]) -> [String]
fourth (_, _, _, x) = x

-- bot -> remaining iterations -> final robot (pos)
iterateBot :: Int -> Robot -> Robot
iterateBot i (Robot pos vec)  | fst pos < 0 = iterateBot i (Robot (fst pos+rows,snd pos) vec)
                              | fst pos >= rows = iterateBot i (Robot (fst pos-rows,snd pos) vec)
                              | snd pos < 0 = iterateBot i (Robot (fst pos, snd pos +cols) vec)
                              | snd pos >= cols = iterateBot i (Robot (fst pos, snd pos-cols) vec)
                              | i == 0 = Robot pos vec
                              | otherwise = iterateBot (i-1) (Robot (fst pos+fst vec, snd pos+snd vec) vec)

-- robot -> min row -> max row -> min col -> max col -> is in quadrant
inQuadrant :: Robot -> Int -> Int -> Int -> Int -> Bool
inQuadrant (Robot pos _) minR maxR minC maxC = fst pos >= minR && fst pos <= maxR && snd pos >= minC && snd pos <= maxC

--position :: Robot -> (Int,Int)
--position (Robot pos _) = pos