import Data.Char (digitToInt)

main :: IO()
main = do
    l <- readFile "app/day9.input"
    let diskmap = map digitToInt l
    let files = expand diskmap 0
    let compacted = compact files (length files-1)
    print (cksum compacted 0 0)

data File = File {id::Maybe Int, len::Int} deriving (Show) -- index is Nothing for empty space, Just Int for file

-- even index digits are File lengths of files
-- index of even is file ID
-- odd index digits are free space
-- 12345 => file id 0 len 1, 2 free, file id 1 len 3, 4 free, file id 2 len 5 
-- input list -> cur index -> output list
expand :: [Int] -> Int -> [File]
expand [] _ = []
expand (n:ns) idx | even idx = File (Just (idx `div` 2)) n : expand ns (idx+1)
                  | odd idx = File Nothing n : expand ns (idx+1)
                  | otherwise = []

-- running file list -> removal index (starts at end of list) -> final file list
compact :: [File] -> Int -> [File]
compact fl i | i == 0 = fl
             | isSpace (fl!!i) = compact fl (i-1)
             | otherwise = compact (replace fl (fl!!i) 0 i) (i-1)

isSpace :: File -> Bool
isSpace (File Nothing _) = True
isSpace _ = False

-- file list -> file to move into space -> inserting index -> max idx to try -> file list after moving
replace :: [File] -> File -> Int -> Int -> [File]
replace fl (File fid flen) insIdx maxIdx | insIdx == maxIdx = fl
                                       | not (isSpace (fl!!insIdx)) || not (fits (fl!!insIdx) (File fid flen)) = replace fl (File fid flen) (insIdx+1) maxIdx
                                       | otherwise = take insIdx fl ++ fillSpace (fl!!insIdx) (File fid flen) ++ drop (insIdx+1) (take maxIdx fl) ++ [File Nothing flen] ++ drop (maxIdx+1) fl

fits :: File -> File -> Bool
fits (File _ sl) (File _ fl) = fl <= sl

-- space File -> file File -> files to replace (could include space)
fillSpace :: File -> File -> [File]
fillSpace (File _ sl) (File fi fl) | fl == sl = [File fi fl]                            -- file fits exactly
                                   | otherwise = [File fi fl, File Nothing (sl-fl)]     -- file fits with some space left over

-- list of files/space -> index -> running cksum -> final cksum
cksum :: [File] -> Int -> Int -> Int
cksum [] _ s = s
cksum ((File Nothing l):fl) i s = cksum fl (i+l) s
cksum ((File (Just fid) l):fl) i s = cksum fl (i+l) (s+score fid l i)

-- file ID -> file len -> index of first block -> score
score :: Int -> Int -> Int -> Int
score fid l firstIdx = sum [i*fid | i <- take l [firstIdx..]]