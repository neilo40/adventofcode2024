import Data.Map (Map)
import qualified Data.Map as Map
import Data.List.Split
import Data.List (sortBy)
import qualified Data.Ord as Ordering


main :: IO()
main = do
    ls <- readLines "app/day5.input"
    let rules = parseRules ls Map.empty
    let updates = parseUpdates ls []
    let unordered = [x | x <- updates, not (allInOrder rules x)]
    let ordered =  map (sortBy (comp rules)) unordered
    print (sum [middle x | x <- ordered])

-- read all lines from file into list
readLines :: FilePath -> IO [String]
readLines fp = lines <$> readFile fp -- <$> is fmap?

comp :: Map Int [Int] -> Int -> Int -> Ordering
comp rules l r | pairInOrder rules l r = Ordering.LT
               | otherwise = Ordering.GT

-- rules -> left -> right -> in order
pairInOrder :: Map Int [Int] -> Int -> Int -> Bool
pairInOrder rules l r = isInOrder rules l [r]

-- lines -> running map -> final map
parseRules :: [String] -> Map Int [Int] -> Map Int [Int]
parseRules (x:_) m | x == "" = m                                                                    -- finish when we get to empty line
parseRules (x:xs) m = parseRules xs (Map.insertWith (++) (head o) (tail o) m) where                 -- add to map
        o = extractOperands x                                                                       --  extracting operands from string
parseRules _ m = m                                                                                  -- for completeness, shouldn't get here

-- extract nums from e.g. 56|78
extractOperands :: String -> [Int]
extractOperands s = [read x | x <- splitOn "|" s]

-- lines -> running list -> final list
parseUpdates :: [String] -> [[Int]] -> [[Int]]
parseUpdates [] us = us                                                                             -- end of input list
parseUpdates (x:xs) us | ',' `elem` x = parseUpdates xs (us ++ [[read n | n <- splitOn "," x]])     -- comma in the line, parse it as an update
parseUpdates (_:xs) us = parseUpdates xs us                                                         -- any other line, skip

allInOrder :: Map Int [Int] -> [Int] -> Bool
allInOrder _ [] = True
allInOrder m (x:xs) | isInOrder m x xs = allInOrder m xs
                    | not (isInOrder m x xs) = False
allInOrder _ _ = False

-- rule map -> update -> in order
isInOrder :: Map Int [Int] -> Int -> [Int] -> Bool
isInOrder m h u = and [x `elem` Map.findWithDefault [] h m | x <- u]

-- return the middle element of the list (always odd length)
middle :: [Int] -> Int
middle xs = xs !! floor (a / 2) where
    a = fromIntegral (length xs) :: Float