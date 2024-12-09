import Data.Char
import Data.Maybe (catMaybes)

main :: IO()
main = do
    l <- readFile "app/day9.input"
    let diskmap = map digitToInt l
    let blocks = expand diskmap 0 0
    let compacted = compact blocks 0 (length blocks-1)
    let vals = catMaybes compacted
    let cksum = checksum (zip vals [0..length vals])
    print cksum

-- even digits are block lengths of files
-- index of even is file ID
-- odd digits are free space
-- 12345 => file id 0 len 1, 2 free, file id 1 len 3, 4 free, file id 2 len 5 
-- input list -> ID -> cur index -> output list
expand :: [Int] -> Int -> Int -> [Maybe Int]
expand [] _ _ = []
expand (n:ns) idnum idx | even idx = replicate n (Just idnum) ++ expand ns (idnum+1) (idx+1)
                        | odd idx = replicate n Nothing ++ expand ns idnum (idx+1)
                        | otherwise = []

-- cur list -> insert at -> remove from -> final list
compact :: [Maybe Int] -> Int -> Int -> [Maybe Int]
compact l insIdx remIdx | insIdx == remIdx = l
                        | hasVal (l!!insIdx) = compact l (insIdx+1) remIdx                         -- skip insIdx forward until next Nothing
                        | not (hasVal (l!!remIdx)) = compact l insIdx (remIdx-1)                   -- skip remIdx backwards until next Value
                        | otherwise = compact (swapTwo insIdx remIdx l) insIdx remIdx              -- swap places and continue

-- Optional Int -> true if val or false if nothing
hasVal :: Maybe Int -> Bool
hasVal (Just _) = True
hasVal Nothing = False

-- TODO: This is way too slow.  Just calculate the running sum as we go instead
-- list -> swap idx 1 -> swap idx 2 -> list
swapTwo :: Int -> Int -> [Maybe Int] -> [Maybe Int]
swapTwo f s xs = zipWith (\x y -> 
    if x == f then xs !! s
    else if x == s then xs !! f
    else y) [0..] xs

checksum :: [(Int,Int)] -> Int
checksum vs = sum [uncurry (*) v | v <- vs]