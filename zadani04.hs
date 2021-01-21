-- Kostra 4. domaci ulohy IB015
-- Po vyreseni cely obsah souboru vlozte do odpovedniku.

-- Nejprve zadefinujte typovy alias Bitvec 
-- (az po jeho definici bude mozne projekt zkompilovat)
-- Bitvec will be composed from list of Boolean, False represent 0, True 1, as usual
type Bitvec = [Bool]

-- Nasledujici funkce mate za ukol implementovat.
-- Convert String to Bitvec
toBitvec :: String -> Bitvec
toBitvec str = [c == '1' | c <- str]

-- Convert Bitvec to String
fromBitvec :: Bitvec -> String
fromBitvec bv = [if x then '1' else '0' | x <- bv]

-- Negate Bitvec
bvnot :: Bitvec -> Bitvec
bvnot = map not

-- Compute and of two Bitvec
bvand :: Bitvec -> Bitvec -> Bitvec
bvand = zipTwoBitvec (&&)

-- Compute or of two Bitvec
bvor :: Bitvec -> Bitvec -> Bitvec
bvor = zipTwoBitvec (||)

-- Compute xor of two Bitvec
bvxor :: Bitvec -> Bitvec -> Bitvec
bvxor = zipTwoBitvec ((==) . not)

-- Prepend diff_length zero Bitvec, drop first diff bits (if diff is negative), diff is (first arg - length of Bitvec)
resize :: Int -> Bitvec -> Bitvec
resize n bv = drop (-diff) 
    (zero diff ++ bv) 
    where diff = n - (length bv)

-- Generate n length zero Bitvec (n is first argument)
zero :: Int -> Bitvec
zero = (flip take) (cycle [False])

-- Rotate Bitvec right (will be cycling, e.g. for n = length will be Bitvec unchanged)
rotRight :: Int -> Bitvec -> Bitvec
rotRight = rotation (-)

-- Rotate Bitvec left (will be cycling, e.g. for n = length will be Bitvec unchanged)
rotLeft :: Int -> Bitvec -> Bitvec
rotLeft = rotation (+)

-- Shift Bitvec right (replace left bits with False)
shiftRight :: Int -> Bitvec -> Bitvec
shiftRight = shift (-)

-- Shift Bitvec left (replace right bits with False)
shiftLeft :: Int -> Bitvec -> Bitvec
shiftLeft = shift (+)

-- Add two Bitvec together, overflow is forgotten
add :: Bitvec -> Bitvec -> Bitvec
add bv1 bv2 = if or bv2
    then add (bvxor bv1 bv2) (shiftLeft 1 (bvand bv1 bv2))
    else resize (getMaxLen bv1 bv2) bv1

-- Return max length of two Bitvec
getMaxLen :: Bitvec -> Bitvec -> Int
getMaxLen bv1 bv2 = max (length bv1) (length bv2)

-- Apply zipWith on two Bitvecs after resizing them to longer Bitvec
zipTwoBitvec :: (Bool -> Bool -> Bool) -> Bitvec -> Bitvec -> Bitvec
zipTwoBitvec f bv1 bv2 = zipWith f (resize m bv1) (resize m bv2) 
    where m = getMaxLen bv1 bv2

-- Rotate Bitvec in desired direction (will be cycling, e.g. for n = length will be Bitvec unchanged)
rotation :: (Int -> Int -> Int) -> Int -> Bitvec -> Bitvec
rotation direction n bv = rotateBitvecWithAffix bv direction n bv

-- Shift Bitvec in desired direction (replace shifted bits with False)
shift :: (Int -> Int -> Int) -> Int -> Bitvec -> Bitvec
shift direction n bv = if n >= l 
    then zero l 
    else rotateBitvecWithAffix (zero l) direction n bv 
    where l = length bv

-- Add prefix and suffix to Bitvec (both have length of Bitvec), then rotate result by n bits in desired direction
rotateBitvecWithAffix :: Bitvec -> (Int -> Int -> Int) -> Int -> Bitvec -> Bitvec
rotateBitvecWithAffix affix direction n bv = take l (
        drop (mod (direction l n) (2*l)) 
        (affix ++ bv ++ affix)
    ) 
    where l = length bv


-- HERE START TESTS
tests :: [(Bool, String)]
tests = [
    (( fromBitvec . zero ) 5 == "00000", "ZERO_5_TIMES"),
    (( fromBitvec . zero ) 0 == "", "ZERO_0_TIMES"),
    (( fromBitvec . resize 5 . toBitvec ) "" == "00000", "RESIZE_TO_LONGER"),
    (( fromBitvec . resize 3 . toBitvec ) "101010" == "010", "RESIZE_TO_SHORTER"),
    (( fromBitvec . resize 5 . toBitvec ) "00010" == "00010", "RESIZE_TO_SAME"),
    (( fromBitvec . toBitvec ) "0010" == "0010", "FROMBITVEC&TOBITVEC_1"),
    (( fromBitvec . toBitvec ) "" == "", "FROMBITVEC&TOBITVEC_EMPTY"),
    (( fromBitvec . toBitvec ) "101010" == "101010", "FROMBITVEC&TOBITVEC_3"),
    (( fromBitvec . bvnot . toBitvec ) "" == "", "BVNOT_EMPTY"),
    (( fromBitvec . bvnot . toBitvec ) "101010" == "010101", "BVNOT_1"),
    ( fromBitvec ( bvand ( toBitvec "1100" ) ( toBitvec "1010" ) ) == "1000", "BVAND_SAME_SIZE"),
    (fromBitvec ( bvand ( toBitvec "1100" ) ( toBitvec "001010" ) ) == "001000", "BVAND_SECOND_LONGER"),
    (fromBitvec ( bvand ( toBitvec "1010" ) ( toBitvec "" ) ) == "0000", "BVAND_FIRST_LONGER"),
    (fromBitvec ( bvand ( toBitvec "" ) ( toBitvec "" ) ) == "", "BVAND_BOTH_EMPTY"),
    (fromBitvec ( bvor ( toBitvec "1100" ) ( toBitvec "1010" ) ) == "1110", "BVOR_SAME_SIZE"),
    (fromBitvec ( bvor ( toBitvec "1100" ) ( toBitvec "001010" ) ) == "001110", "BVOR_SECOND_LONGER"),
    (fromBitvec ( bvor ( toBitvec "1010" ) ( toBitvec "" ) ) == "1010", "BVOR_FIRST_LONGER"),
    (fromBitvec ( bvor ( toBitvec "" ) ( toBitvec "" ) ) == "", "BVOR_BOTH_EMPTY"),
    (fromBitvec ( bvxor ( toBitvec "1100" ) ( toBitvec "1010" ) ) == "0110", "BVXOR_SAME_SIZE"),
    (fromBitvec ( bvxor ( toBitvec "1100" ) ( toBitvec "001010" ) ) == "000110", "BVXOR_SECOND_LONGER"),
    (fromBitvec ( bvxor ( toBitvec "1010" ) ( toBitvec "" ) ) == "1010", "BVXOR_FIRST_LONGER"),
    (fromBitvec ( bvxor ( toBitvec "" ) ( toBitvec "" ) ) == "", "BVXOR_BOTH_EMPTY"),
    (( fromBitvec . rotRight 2 . toBitvec ) "1011100" == "0010111", "ROTRIGHT_SMALLER_THAN_LENGTH"),
    (( fromBitvec . rotRight 0 . toBitvec ) "10110010" == "10110010", "ROTRIGHT_ZERO_NON_EMPTY_BITVEC"),
    (( fromBitvec . rotRight 21 . toBitvec ) "0001" == "1000", "ROTRIGHT_BIGGER_THAN_LENGTH"),
    (( fromBitvec . rotRight 0 . toBitvec ) "" == "", "ROTRIGHT_ZERO_EMPTY_BITVEC"),
    (( fromBitvec . rotLeft 2 . toBitvec ) "1011100" == "1110010", "ROTLEFT_SMALLER_THAN_LENGTH"),
    (( fromBitvec . rotLeft 0 . toBitvec ) "10110010" == "10110010", "ROTLEFT_ZERO_NON_EMPTY_BITVEC"),
    (( fromBitvec . rotLeft 21 . toBitvec ) "0001" == "0010", "ROTLEFT_BIGGER_THAN_LENGTH"),
    (( fromBitvec . rotLeft 0 . toBitvec ) "" == "", "ROTLEFT_ZERO_EMPTY_BITVEC"),
    (( fromBitvec . shiftRight 2 . toBitvec ) "1011100" == "0010111", "SHIFTRIGHT_SMALLER_THAN_LENGTH"),
    (( fromBitvec . shiftRight 4 . toBitvec ) "1011" == "0000", "SHIFTRIGHT_EQUAL_LENGTH"),
    (( fromBitvec . shiftRight 3 . toBitvec ) "1011" == "0001", "SHIFRIGHT_ALMOST_LENGTH"),
    (( fromBitvec . shiftRight 0 . toBitvec ) "10110010" == "10110010", "SHIFTRIGHT_ZERO_NON_EMPTY_BITVEC"),
    (( fromBitvec . shiftRight 21 . toBitvec ) "0001" == "0000", "SHIFTRIGHT_BIGGER_THAN_LENGTH"),
    (( fromBitvec . shiftRight 0 . toBitvec ) "" == "", "SHIFTRIGHT_ZERO_EMPTY_BITVEC"),
    (( fromBitvec . shiftLeft 2 . toBitvec ) "1011100" == "1110000", "SHIFTLEFT_SMALLER_THAN_LENGTH"),
    (( fromBitvec . shiftLeft 4 . toBitvec ) "1011" == "0000", "SHIFTLEFT_EQUAL_LENGTH"),
    (( fromBitvec . shiftLeft 3 . toBitvec ) "1011" == "1000", "SHIFTLEFT_ALMOST_LENGTH"),
    (( fromBitvec . shiftLeft 0 . toBitvec ) "10110010" == "10110010", "SHIFTLEFT_ZERO_NON_EMPTY_BITVEC"),
    (( fromBitvec . shiftLeft 21 . toBitvec ) "0001" == "0000", "SHIFTLEFT_BIGGER_THAN_LENGTH"),
    (( fromBitvec . shiftLeft 0 . toBitvec ) "" == "", "SHIFTLEFT_ZERO_EMPTY_BITVEC"),
    (fromBitvec ( add ( toBitvec "1000" ) ( toBitvec "0010" ) ) == "1010", "ADD_SAME_SIZE_NON_CARRY_OVER"),
    (fromBitvec ( add ( toBitvec "1010" ) ( toBitvec "0010" ) ) == "1100", "ADD_SAME_SIZE_CARRY_OVER"),
    (fromBitvec ( add ( toBitvec "1010" ) ( toBitvec "1010" ) ) == "0100", "ADD_SAME_SIZE_OVERFLOW"),
    (fromBitvec ( add ( toBitvec "1010" ) ( toBitvec "001010" ) ) == "010100", "ADD_SECOND_LONGER_CARRY_OVER"),
    (fromBitvec ( add ( toBitvec "001010" ) ( toBitvec "1010" ) ) == "010100", "ADD_FIRST_LONGER_CARRY_OVER"),
    (fromBitvec ( add ( toBitvec "001010" ) ( toBitvec "" ) ) == "001010", "ADD_SECOND_EMPTY"),
    (fromBitvec ( add ( toBitvec "0" ) ( toBitvec "" ) ) == "0", "ADD_SECOND_EMPTY_2"),
    (fromBitvec ( add ( toBitvec "111111111" ) ( toBitvec "000000000000" ) ) == "000111111111", "ADD_SECOND_ONLY_NULL_AND_LONGER"),
    (fromBitvec ( add ( toBitvec "" ) ( toBitvec "0" ) ) == "0", "ADD_FIRST_EMPTY"),
    (fromBitvec ( add ( toBitvec "111111111111" ) ( toBitvec "001" ) ) == "000000000000", "ADD_OVERFLOW_TO_NULL"),
    (fromBitvec ( add ( toBitvec "" ) ( toBitvec "" ) ) == "", "ADD_BOTH_EMPTY")]

runTests :: [(Bool, String)] -> IO ()
runTests [] = putStrLn "ALL TESTS PASSED!"
runTests ((test, err):s) = if (not test) then putStrLn ("ERROR: " ++ err) else runTests s
