main :: IO()
main = do
    ls <- readLines "day2.input"
    print (countIncOrDec (numerifyLines ls))

-- read all lines from file into list
readLines :: FilePath -> IO [String]
readLines fp = lines <$> readFile fp -- <$> is fmap?

-- convert all lines to list of lists of Ints
numerifyLines :: [String] -> [[Int]]
numerifyLines ls = [numerify l | l <- ls]

-- convert single input line into a list of ints
numerify :: String -> [Int]
numerify s = [read x | x <- words s]

-- count how many sequences increase or decrease appropriately
countIncOrDec :: [[Int]] -> Int
countIncOrDec nns = sum [1 | ns <- nns, incOrDec ns]

-- is sequence increasing or decreasing
incOrDec :: [Int] -> Bool
incOrDec ns = increase ns [] False || decrease ns [] False

-- is sequence increasing?
-- good lord, the jankiness.
-- takes list of numbers left to eval, previously evaluated nums, and whether to allow further failures
increase :: [Int] -> [Int] -> Bool -> Bool
increase [_] _ _ = True
increase (x:xs) _ True = deltaPlus [x, head xs] && increase xs [] True
-- if deltaPlus fails, try both alternatives (remove first or second of the pair) and continue, disallowing any more removals
increase (x:xs) prevs False = (deltaPlus [x, head xs] && increase xs (prevs ++ [x]) False) || increase (prevs ++ [x] ++ tail xs) [] True || increase (prevs ++ xs) [] True

-- check how much values changed by.  must be increasing by 1,2, or 3 only
deltaPlus :: [Int] -> Bool
deltaPlus ns = delta ns > 0 && delta ns < 4

-- is sequence decreasing?
decrease :: [Int] -> [Int] -> Bool -> Bool
decrease [_] _ _ = True
decrease (x:xs) _ True = deltaMinus [x, head xs] && decrease xs [] True
decrease (x:xs) prevs False = (deltaMinus [x, head xs] && decrease xs (prevs ++ [x]) False) || decrease (prevs ++ [x] ++ tail xs) [] True || decrease (prevs ++ xs) [] True

-- check how much values changed by.  must be decreasing by 1,2, or 3 only
deltaMinus :: [Int] -> Bool
deltaMinus ns = delta ns < 0 && delta ns > -4

-- calculate the delta between two values
delta :: [Int] -> Int
delta ns = ns !! 1 - head ns
