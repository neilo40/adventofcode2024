import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Regex.TDFA

data Gate = Gate String String (Bool->Bool->Bool) 
instance Show Gate where
    show (Gate i1 i2 _) = show i1 ++ "," ++ show i2

main :: IO()
main = do
    ls <- readLines "app/day24.input"
    let initialValues = takeWhile (/= "") ls
    let gates = dropWhile isNotGate ls
    let im = makeInitialMap initialValues Map.empty
    let fm = makeFinalMap gates im
    let zvals = [boolToInt (valueForWire fm w) | w <- Map.keys fm, head w == 'z']
    let zzvals = zip zvals [0..]
    let num = sum [fst x * 2 ^ snd x | x <- zzvals]
    print num

readLines :: FilePath -> IO [String]
readLines fp = lines <$> readFile fp 

isNotGate :: String -> Bool
isNotGate s = not ("->" `isInfixOf` s)

-- put the initial values into the map
makeInitialMap :: [String] -> Map String Gate -> Map String Gate
makeInitialMap [] m = m
makeInitialMap (l:ls) m | val = makeInitialMap ls (Map.insert wire (Gate "" "" alwaysTrue) m)
                        | otherwise = makeInitialMap ls (Map.insert wire (Gate "" "" alwaysFalse) m)
                        where
                            val = boolForChar (last l)
                            wire = take 3 l

-- now add all the real gates to the map
makeFinalMap :: [String] -> Map String Gate -> Map String Gate
makeFinalMap [] m = m
makeFinalMap (l:ls) m = makeFinalMap ls (Map.insert (last matches) (Gate (head matches) (matches!!2) (opFromString (matches!!1))) m)
                        where
                            pat = "([a-z0-9]{3}) (OR|XOR|AND) ([a-z0-9]{3}) -> ([a-z0-9]{3})"
                            matches = fourth (l =~ pat :: (String,String,String,[String]))

-- get the 4th value from the 4-tuple returned by the regex match
fourth :: (String, String, String, [String]) -> [String]
fourth (_, _, _, x) = x

opFromString :: String -> (Bool -> Bool -> Bool)
opFromString s  | s == "OR" = (||)
                | s == "XOR" = (/=)
                | s == "AND" = (&&)
                | otherwise = (||)

boolForChar :: Char -> Bool
boolForChar c | c == '1' = True
              | otherwise = False

alwaysTrue :: Bool -> Bool -> Bool 
alwaysTrue _ _ = True

alwaysFalse :: Bool -> Bool -> Bool 
alwaysFalse _ _ = False

valueForGate :: Map String Gate -> Gate -> Bool
valueForGate m (Gate i1 i2 op) | i1 == "" = op True True
                               | otherwise = op (valueForGate m (m Map.! i1)) (valueForGate m (m Map.! i2)) 

valueForWire :: Map String Gate -> String -> Bool
valueForWire m w = valueForGate m (m Map.! w)

boolToInt :: Bool -> Int
boolToInt b | b = 1
            | otherwise = 0