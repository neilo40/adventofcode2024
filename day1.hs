import System.IO
import Data.List (sort)

main :: IO()
main = do
    -- really need to understand the IO monad and it's taint

    ls <- readLines "day1.input" -- what is <- here and how is it different from let?  seems to remove the IO bit...

    -- part a    
    print (sum (distance (zipLines ls)))

    -- part b
    print (sum (score (makeList ls 0) (makeList ls 1)))

-- read all lines from file into list
readLines :: FilePath -> IO [String]
readLines fp = lines <$> readFile fp -- <$> is fmap?

-- make a list of tuples from the same row of each column
zipLines :: [String] -> [(Int,Int)]
zipLines ls = zip (makeList ls 0) (makeList ls 1)

-- make a sorted list of all numbers in column 0 or 1
makeList :: [String] -> Int -> [Int]
makeList ls i = sort [numerify l !! i | l <- ls]

-- convert each input line into a list of ints
numerify :: String -> [Int]
numerify s = [read x | x <- words s]

-- get the distance between each value in the tuple
distance :: [(Int, Int)] -> [Int]
distance ts = [abs (uncurry (-) t) | t <- ts]

-- get number of times value appears in list
freq :: [Int] -> Int -> Int
freq xs v = sum [1 | x <- xs, x == v]

-- for each val in first list, multiply by freq in 2nd list
score :: [Int] -> [Int] -> [Int]
score as bs = [a * freq bs a | a <- as]