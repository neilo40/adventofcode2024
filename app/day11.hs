main :: IO()
main = do
    l <- readFile "app/day11.input"
    let start = parseInput l
    print (length (applyRulesN start 75))

parseInput :: String -> [Int]
parseInput l = map read (words l)

applyRulesN :: [Int] -> Int -> [Int]
applyRulesN ns c | c == 0 = ns
                 | otherwise = applyRulesN (applyRules ns) (c-1)

applyRules :: [Int] -> [Int]
applyRules = concatMap applyRule

applyRule :: Int -> [Int]
applyRule n | n == 0 = [1]
            | even l = [read (fst newNums), read (snd newNums)] 
            | otherwise = [n * 2024]
            where
                newNums = splitAt ((l +1) `div` 2) (show n)
                l = length (show n)
