import Data.List.Split

main :: IO()
main = do
    ls <- readLines "app/day7.input"
    let possiblyTrue = possiblyTrueLines [parseLine l | l <- ls]
    let s = sum [fst l | l <- possiblyTrue]
    print s

-- read all lines from file into list
readLines :: FilePath -> IO [String]
readLines fp = lines <$> readFile fp 

-- line -> (total, operands)
parseLine :: String -> (Int,[Int])
parseLine l = (read (head parts), [read n | n <- words (last parts)]) where 
    parts = splitOn ":" l

-- target -> remaining operands -> running total -> was possible
isPossiblyTrue :: Int -> [Int] -> Int -> Bool
isPossiblyTrue t [] s = t == s                                                                                      -- no more operands and sum == target, success
isPossiblyTrue t _ s | s > t = False                                                                                -- sum exceeds target, fail immediately
isPossiblyTrue t ops s = isPossiblyTrue t (tail ops) (s + head ops) || isPossiblyTrue t (tail ops) (s * head ops)   -- try + or * with next operand

possiblyTrueLines :: [(Int,[Int])] -> [(Int,[Int])]
possiblyTrueLines ls = [l | l <- ls, uncurry isPossiblyTrue l 0]
