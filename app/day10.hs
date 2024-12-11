import Data.Set (Set)
import qualified Data.Set as Set
import Data.Char
import Data.Maybe (mapMaybe)

data Point = Point Int Int deriving (Show,Eq,Ord)
maxDim :: Int
maxDim = 54

main :: IO()
main = do
    g <- readLines "app/day10.input"
    let trailheads = Set.fromList (starting g (Point 0 0))
    print (allSummitCounts g trailheads)

-- read all lines from file into list
readLines :: FilePath -> IO [String]
readLines fp = lines <$> readFile fp 

-- grid -> position -> Set of starting points (height == 0)
starting :: [String] -> Point -> [Point]
starting g (Point row col)  | col > maxDim = starting g (Point (row+1) 0)
                            | row > maxDim = []
                            | height g (Point row col) == 0 = Point row col : starting g (Point row (col+1))
                            | otherwise = starting g (Point row (col+1))

-- grid -> set of starting points -> total paths to summits
allSummitCounts :: [String] -> Set Point -> Int
allSummitCounts g ps = sum [summitCount g (Set.fromList [p]) 0 Set.empty | p <- Set.elems ps]

-- grid -> current points -> current height -> summits -> final summit count
summitCount :: [String] -> Set Point -> Int -> Set Point -> Int
summitCount g ps h sc   | ps == Set.empty = length sc
                        | otherwise = summitCount g nextps (h+1) (Set.union sc (Set.filter (isSummit g) nextps)) where
                            nextps = iter g ps h

-- grid -> pos -> is summit (height == 9)
isSummit :: [String] -> Point -> Bool
isSummit g pos = height g pos == 9

-- grid -> set of current points -> current height -> next set of points
iter :: [String] -> Set Point -> Int -> Set Point
iter g ps h = Set.fromList (concat [nextPoints g h p | p <- Set.elems ps])

-- grid -> position -> height -> next points 
nextPoints :: [String] -> Int -> Point -> [Point]
nextPoints g h pos = [p | p <- eligiblePoints pos, height g p == h+1]

eligiblePoints :: Point -> [Point]
eligiblePoints pos = mapMaybe (point pos) [Point 0 1, Point 0 (-1), Point 1 0, Point (-1) 0]

-- position -> direction -> point
point :: Point -> Point -> Maybe Point
point (Point row col) (Point rd cd) | row+rd < 0 || col+cd < 0 || row+rd > maxDim || col+cd > maxDim = Nothing
                                    | otherwise = Just (Point (row+rd) (col+cd))

-- grid -> pos -> height
height :: [String] -> Point -> Int
height g (Point row col) = digitToInt ((g!!row)!!col)