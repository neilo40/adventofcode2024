--import Data.Set (Set)
import qualified Data.Set as Set

main :: IO()
main = do
    grid <- readLines "app/day6.input"
    let sp = getStartPos grid 0 0
    let been = iterateMap grid sp (-1, 0) []
    let uniqueBeen = Set.fromList been
    print (Set.size uniqueBeen)

-- read all lines from file into list
readLines :: FilePath -> IO [String]
readLines fp = lines <$> readFile fp -- <$> is fmap?

-- grid -> row -> col -> starting position
getStartPos :: [String] -> Int -> Int -> (Int,Int)
getStartPos grid row col | col >= length (grid!!row) = getStartPos grid (row+1) 0 
                       | (grid!!row)!!col == '^' = (row,col)
getStartPos grid row col = getStartPos grid row (col+1)

-- grid -> (row,col) -> (row dir,col dir) -> running been -> been
iterateMap :: [String] -> (Int, Int) -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
iterateMap _ pos _ been | fst pos < 0 || fst pos > 129 || snd pos < 0 || snd pos > 129 = been                               -- out of bounds, end
iterateMap grid pos dir been | nextCell grid pos dir == '#' = iterateMap grid (move pos newdir) newdir been++[pos] where    -- obstacle, turn and move
                                                        newdir = turnRight dir
iterateMap grid pos dir been = iterateMap grid (move pos dir) dir been++[pos]                                               -- empty space, just move

-- grid -> pos -> char at pos
cell :: [String] -> (Int,Int) -> Char
cell grid pos = (grid!!fst pos)!!snd pos

-- grid -> pos -> dir -> char at next pos
nextCell :: [String] -> (Int,Int) -> (Int,Int) -> Char
nextCell _ pos dir | fst newpos < 0 || fst newpos > 129 || snd newpos < 0 || snd newpos > 129 = '.' where
                        newpos = move pos dir
nextCell grid pos dir = cell grid (move pos dir)

-- dir -> new dir
turnRight :: (Int, Int) -> (Int, Int)
turnRight dir   | dir == (0, 1) = (1, 0)    -- right -> down
                | dir == (1, 0) = (0, -1)   -- down -> left
                | dir == (0, -1) = (-1, 0)  -- left -> up
                | dir == (-1, 0) = (0, 1)   -- up -> right
turnRight dir = dir                         -- invalid, completeness

-- pos -> dir --> new pos
move :: (Int, Int) -> (Int, Int) -> (Int, Int)
move pos dir = (fst pos + fst dir, snd pos + snd dir)