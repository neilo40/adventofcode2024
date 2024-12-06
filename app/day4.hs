main :: IO()
main = do
    content <- readFile "app/day4.input"
    let grid = lines content
    let w = extractWords grid 0 0 [[]]
    let xmases = [x | x <- w, x == ['X','M','A','S']]
    --print xmases
    print (length xmases)

-- grid -> row -> col -> found words -> final list
extractWords :: [[Char]] -> Int -> Int -> [[Char]] -> [[Char]]
extractWords _ row col l | row == 139 && col == 140 = l                                                                 -- end of grid
extractWords grid row col l | col == 140 = extractWords grid (row+1) 0 l                                                -- end of row
extractWords grid row col l | (grid!!row)!!col == 'X' = extractWords grid row (col+1) (l++wordsForPos grid row col)     -- found X, gather all words from here
extractWords grid row col l = extractWords grid row (col+1) l                                                           -- not X, continue

-- grid -> row -> col -> found words
wordsForPos :: [[Char]] -> Int -> Int -> [[Char]]
wordsForPos grid row col = [find grid row col 1 0 []] ++ [find grid row col (-1) 0 []] ++ [find grid row col 0 1 []] ++ [find grid row col 0 (-1) []] ++ [find grid row col 1 1 []] ++ [find grid row col (-1) (-1) []] ++ [find grid row col (-1) 1 []] ++ [find grid row col 1 (-1) []]

-- grid -> row -> col -> row dir -> col dir -> found letters -> final word
find :: [[Char]] -> Int -> Int -> Int -> Int -> [Char] -> [Char]
find _ row col _ _ l | row < 0 || row > 139 || col < 0 || col > 139 = l                     -- out of bounds on col or row
find _ _ _ _ _ l | length l > 3 = l                                                         -- found 4 letters
find grid row col rd cd l = find grid (row+rd) (col+cd) rd cd (l++[(grid!!row)!!col])       -- add current letter and move to the next