main :: IO()
main = do
    ls <- readLines "app/day19.input"
    let towels = parseTowels (head ls)
    let designs = drop 2 ls
    let possible = sum [isPossible towels d| d <- designs]
    print possible

readLines :: FilePath -> IO [String]
readLines fp = lines <$> readFile fp 

parseTowels :: String -> [String]
parseTowels l = map (takeWhile isNotComma) (words l)

isNotComma :: Char -> Bool
isNotComma c = c /= ','

-- towels -> design -> possible combos -> final possible combos
--isPossible :: [String] -> String -> [String] -> [[String]]
--isPossible _ [] combos = [combos]                                         
--isPossible ts d combos  | null poss = []                            
--                        | otherwise = concat [isPossible ts (drop (length p) d) (combos++[p]) | p <- poss]
--                where                       
--    poss = [t | t <- ts, hasPrefix d t] 
-- towels -> design -> is possible

isPossible :: [String] -> String -> Int
isPossible _ [] = 1                                                     -- nothing remaining in design, it is possible to create it
isPossible ts d | null poss = 0                                         -- none were possible, end with false
                | otherwise = sum [isPossible ts r | r <- remainders]   -- some were possible, iterate
                where                       
    poss = [t | t <- ts, hasPrefix d t] 
    remainders = [drop (length p) d | p <- poss]

hasPrefix :: String -> String -> Bool 
hasPrefix s p = take (length p) s == p 
