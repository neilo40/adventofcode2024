main :: IO()
main = do
    ls <- readLines "app/day19.input"
    let towels = parseTowels (head ls)
    let designs = drop 2 ls
    let possible = [d | d <- designs, isPossible towels d]
    print (length possible)

readLines :: FilePath -> IO [String]
readLines fp = lines <$> readFile fp 

parseTowels :: String -> [String]
parseTowels l = map (takeWhile isNotComma) (words l)

isNotComma :: Char -> Bool
isNotComma c = c /= ','

-- towels -> design -> is possible
isPossible :: [String] -> String -> Bool
isPossible _ [] = True                                          -- nothing remaining in design, it is possible to create it
isPossible ts d | null poss = False                             -- none were possible, end with false
                | otherwise = any (isPossible ts) remainders    -- some were possible, iterate
                where                       
    poss = [t | t <- ts, hasPrefix d t] 
    remainders = [drop (length p) d | p <- poss]

hasPrefix :: String -> String -> Bool 
hasPrefix s p = take (length p) s == p 
