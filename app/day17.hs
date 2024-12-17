import Data.Bits

main :: IO()
main = do
    let prog = [2,4,1,3,7,5,4,0,1,3,0,3,5,5,3,0]
    --let prog = [0,1,5,4,3,0]
    print (runProg prog 0 (Reg 51342988 0 0) [])
    --print (runProg prog 0 (Reg 729 0 0) [])

data Reg = Reg Int Int Int deriving (Show)

-- program -> pc -> registers -> running output -> output
runProg :: [Int] -> Int -> Reg -> [Int] -> [Int]
runProg prg pc reg o | pc >= length prg = o
                     | otherwise = runProg prg newpc newreg newo  where
    oc = prg!!pc
    op = prg!!(pc+1)
    (newreg,newpc,newo) = runInstruction op reg oc pc o

-- operand -> input registers -> opcode -> pc -> running output -> (new registers, new pc, new outputs)
runInstruction :: Int -> Reg -> Int -> Int -> [Int] -> (Reg, Int, [Int])
runInstruction v (Reg a b c) oc pc o | oc == 0 = (Reg divA b c, pc+2, o) -- adv
                                     | oc == 1 = (Reg a (b `xor` v) c, pc+2, o) -- bxl
                                     | oc == 2 = (Reg a mod8 c, pc+2, o) -- bst
                                     | oc == 3 && a /= 0 = (Reg a b c, v, o) -- jnz 
                                     | oc == 4 = (Reg a (b `xor` c) c, pc+2, o) -- bxc
                                     | oc == 5 = (Reg a b c, pc+2, o++[mod8]) -- out
                                     | oc == 6 = (Reg a divA c, pc+2, o) -- bdv
                                     | oc == 7 = (Reg a b divA, pc+2, o) -- cdv
                                     | otherwise = (Reg a b c, pc+2, o) where
    divA = a `div` (2 ^ comboOperandValue v (Reg a b c))
    mod8 = comboOperandValue v (Reg a b c) `mod` 8

-- input operand -> registers -> final operand value
comboOperandValue :: Int -> Reg -> Int 
comboOperandValue v (Reg a b c) | v >= 0 && v <= 3 = v
                                | v == 4 = a
                                | v == 5 = b
                                | v == 6 = c
                                | otherwise = 7 -- this should never happen


