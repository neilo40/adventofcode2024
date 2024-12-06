import Text.Regex.TDFA

main :: IO()
main = do
    content <- readFile "app/day3.input"
    print (calcProduct (getInstructions content) True 0)

-- extract all the relevant instructions from the input
getInstructions :: String -> [String]
getInstructions c = getAllTextMatches(c =~ pat) :: [String] where
    pat = "do\\(\\)|don't\\(\\)|mul\\([0-9]+,[0-9]+\\)"

-- calcProduct :: matches -> enabled -> running sum -> sum 
calcProduct :: [String] -> Bool -> Int -> Int
calcProduct [] _ s = s                                                                          -- empty list -> end, return running sum
calcProduct (x:xs) _ s  | x == "do()" = calcProduct xs True s                                   -- enable and continue with next match
                        | x == "don't()" = calcProduct xs False s                               -- disable and continue with next match
calcProduct (_:xs) False s = calcProduct xs False s                                             -- skip and continue with next match
calcProduct (x:xs) True s = calcProduct xs True s+product (convertToNums (extractOperands x))   -- calculate product, add to running sum, continue

-- converts list of string into list of ints
convertToNums :: [String] -> [Int]
convertToNums ns = [read n | n <- ns]

-- match and extract the numerical operands from the mul instruction, 4th element of tuple is all sub-matches
extractOperands :: String -> [String]
extractOperands s = fourth (s =~ pat :: (String, String, String, [String])) where
    pat = "mul\\(([0-9]+),([0-9]+)\\)"

-- get the 4th value from the 4-tuple returned by the regex match
fourth :: (String, String, String, [String]) -> [String]
fourth (_, _, _, x) = x
