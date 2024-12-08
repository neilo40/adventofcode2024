import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List 

main :: IO()
main = do
    ls <- readLines "app/day8.input"
    let fm = allFrequencies ls (Point 0 0) Map.empty            -- locations of all antennas
    let combs = Map.map combinations fm                         -- combinations (pairs) between all antennas of same freq
    let pairs = Map.foldr (++) [] combs                         -- single list of all pairs for all freqs
    let ans = [antinodes p | p <- pairs]                        -- all antinodes
    let validAns = [a | an <- ans, a <- an, isValidAntinode a]  -- filter antinodes that are out of bounds
    let unique = Set.fromList validAns                          -- remove duplicates
    print (length unique)

data Point = Point Int Int deriving (Show,Eq,Ord)

-- read all lines from file into list
readLines :: FilePath -> IO [String]
readLines fp = lines <$> readFile fp 

-- grid -> pos -> seen chars -> final seen chars
allFrequencies :: [String] -> Point -> Map Char [Point] -> Map Char [Point]
allFrequencies grid (Point row col) seen | row > 49 = seen
                                         | col > 49 = allFrequencies grid (Point (row+1) 0) seen
                                         | freqAtPos grid (Point row col) == '.' = allFrequencies grid (Point row (col+1)) seen
                                         | otherwise = allFrequencies grid (Point row (col+1)) (Map.insertWith (++) (freqAtPos grid pos) [pos] seen) where 
                                            pos = Point row col

-- grid -> pos -> char at pos
freqAtPos :: [String] -> Point -> Char
freqAtPos grid (Point row col) = (grid!!row)!!col

-- list of points -> list of all possible point pairs
combinations :: [Point] -> [(Point,Point)]
combinations ps = [(x,y) | (x:ys) <- tails ps, y <- ys]

-- point pair -> two antinodes
antinodes :: (Point,Point) -> [Point]
antinodes (Point row1 col1,Point row2 col2) = [antinode p2 (row2-row1) (col2-col1), antinode p1 (row1-row2) (col1-col2)] where
    p1 = Point row1 col1
    p2 = Point row2 col2

-- point -> row delta -> col delta -> antinode point
antinode :: Point -> Int -> Int -> Point
antinode (Point row col) rd cd = Point (row+rd) (col+cd)

-- point -> is valid (in bounds)
isValidAntinode :: Point -> Bool
isValidAntinode (Point row col) = row >= 0 && row < 50 && col >= 0 && col < 50
