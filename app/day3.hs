import Text.Regex.TDFA

main :: IO()
main = do
    content <- readFile "app/day3.input"
    let pat = "mul\\([0-9]+,[0-9]+\\)"
    let matches = getAllTextMatches(content =~ pat) :: [String]
    print (calcProduct matches)

-- get the 4th value from the 4-tuple returned by the regex match
fourth :: (String, String, String, [String]) -> [String]
fourth (_, _, _, x) = x

-- match and extract the numerical operands from the mul instruction, 4th element of tuple is all sub-matches
extractOperands :: String -> [String]
extractOperands s = fourth(s =~ pat :: (String, String, String, [String])) where
    pat = "mul\\(([0-9]+),([0-9]+)\\)"

-- converts list of string into list of ints
convertToNums :: [String] -> [Int]
convertToNums ns = [read n | n <- ns]

-- simply sum the product of each pair of extracted operands from all matches
calcProduct :: [String] -> Int
calcProduct ms = sum [product (convertToNums (extractOperands m)) | m <- ms]



