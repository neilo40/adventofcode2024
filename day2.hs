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
incOrDec ns = increase ns || decrease ns

-- is sequence increasing?
increase :: [Int] -> Bool
increase [_] = True
increase (x:xs) = deltaPlus [x, head xs] && increase xs 

-- check how much values changed by.  must be increasing by 1,2, or 3 only
deltaPlus :: [Int] -> Bool
deltaPlus ns = delta ns > 0 && delta ns < 4

-- is sequence decreasing?
decrease :: [Int] -> Bool
decrease [_] = True
decrease (x:xs) = deltaMinus [x, head xs] && decrease xs

-- check how much values changed by.  must be decreasing by 1,2, or 3 only
deltaMinus :: [Int] -> Bool
deltaMinus ns = delta ns < 0 && delta ns > -4

-- calculate the delta between two values
delta :: [Int] -> Int
delta ns = ns !! 1 - head ns
