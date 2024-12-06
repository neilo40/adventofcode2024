main :: IO()
main = do
    content <- readFile "app/day4.input"
    let grid = lines content
    print (length [x | x <- getAllXmas grid 0 0 [], x])

-- grid -> row -> col -> found words -> final list
getAllXmas :: [[Char]] -> Int -> Int -> [Bool] -> [Bool]
getAllXmas _ row col found | row == 139 && col == 140 = found                                                               -- end of grid
getAllXmas grid row col found | col == 140 = getAllXmas grid (row+1) 0 found                                                -- end of row
getAllXmas grid row col found | (grid!!row)!!col == 'A' = getAllXmas grid row (col+1) (found++[hasXmas grid row col])       -- found A, look for X-MAS
getAllXmas grid row col found = getAllXmas grid row (col+1) found                                                           -- not A, continue

-- grid -> row -> col -> found X-MAS
hasXmas :: [[Char]] -> Int -> Int -> Bool
hasXmas grid row col = (find grid (row-1) (col-1) 1 1 [] == "MAS" || find grid (row-1) (col-1) 1 1 [] == "SAM") && (find grid (row+1) (col-1) (-1) 1 [] == "MAS" || find grid (row+1) (col-1) (-1) 1 [] == "SAM")

-- grid -> row -> col -> row dir -> col dir -> found letters -> final word
find :: [[Char]] -> Int -> Int -> Int -> Int -> [Char] -> [Char]
find _ row col _ _ l | row < 0 || row > 139 || col < 0 || col > 139 = l                     -- out of bounds on col or row
find _ _ _ _ _ l | length l > 2 = l                                                         -- found 3 letters
find grid row col rd cd l = find grid (row+rd) (col+cd) rd cd (l++[(grid!!row)!!col])       -- add current letter and move to the next
