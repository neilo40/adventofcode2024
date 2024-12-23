import Data.Char

main :: IO()
main = do
    l <- readFile "app/day9.input"
    let diskmap = map digitToInt l
    let blocks = expand diskmap 0 0
    let cksum = compact blocks 0 (length blocks-1) 0
    print cksum

-- even index digits are block lengths of files
-- index of even is file ID
-- odd index digits are free space
-- 12345 => file id 0 len 1, 2 free, file id 1 len 3, 4 free, file id 2 len 5 
-- input list -> ID -> cur index -> output list
expand :: [Int] -> Int -> Int -> [Maybe Int]
expand [] _ _ = []
expand (n:ns) idnum idx | even idx = replicate n (Just idnum) ++ expand ns (idnum+1) (idx+1)
                        | odd idx = replicate n Nothing ++ expand ns idnum (idx+1)
                        | otherwise = []

-- cur list -> insert at -> remove from -> running sum -> final sum
compact :: [Maybe Int] -> Int -> Int -> Int -> Int
compact l insIdx remIdx s | insIdx == remIdx = s+(insIdx * val (l!!insIdx))                                 -- when indexes are the same, we're done
                          | hasVal (l!!insIdx) = compact l (insIdx+1) remIdx s+(insIdx * val (l!!insIdx))   -- skip insIdx forward until next Nothing, adding as we go
                          | not (hasVal (l!!remIdx)) = compact l insIdx (remIdx-1) s                        -- skip remIdx backwards until next Value
                          | otherwise = compact l (insIdx+1) (remIdx-1) s+(insIdx * val (l!!remIdx))        -- add the product and continue

-- Optional Int -> true if val or false if nothing
hasVal :: Maybe Int -> Bool
hasVal (Just _) = True
hasVal Nothing = False

-- is there a better way to get value or else from optional?
val :: Maybe Int -> Int
val (Just v) = v
val Nothing = 0 -- should never get here