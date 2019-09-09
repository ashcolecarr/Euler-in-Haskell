-- Project Euler Problem 33
import Data.Time (getCurrentTime, diffUTCTime)
import Data.Tuple (swap)

main = do
    start <- getCurrentTime
    putStrLn $ show ((finalDenominator . multiplyFractions . getCuriousFractions) fractions)
    stop <- getCurrentTime
    putStrLn $ "Program execution took " ++ show (diffUTCTime stop start) ++ " seconds."

ten :: Int
ten = 10

minLimit :: Int
minLimit = 10

maxLimit :: Int
maxLimit = 99

{- All fractions must be < 1.
   The numerator and denominator are two digits. 
   Nothing evenly divisible by 10 is allowed.
   The fractions without common digits are excluded. 
-}
fractions :: [(Int, Int)]
fractions = filter (hasCommonDigit) $ map (swap) [(denominator', numerator') | denominator' <- [minLimit..maxLimit], numerator' <- [minLimit..maxLimit], numerator' < denominator', denominator' `rem` ten > 0 && numerator' `rem` ten > 0]

floatEq :: (Fractional a, Ord a) => a -> a -> Bool
floatEq a b = 1e-6 > abs (a - b)

hasCommonDigit :: (Int, Int) -> Bool
hasCommonDigit (num, denom)
    | num `quot` ten == denom `rem` ten = True
    | num `rem` ten == denom `quot` ten = True
    | otherwise = False

getCuriousFractions :: [(Int, Int)] -> [(Int, Int)]
getCuriousFractions [] = []
getCuriousFractions ((num, denom):fracs)
    | (fromIntegral num / fromIntegral denom) `floatEq` (fromIntegral (num `quot` ten) / fromIntegral (denom `rem` ten)) = (num, denom) : getCuriousFractions fracs
    | (fromIntegral num / fromIntegral denom) `floatEq` (fromIntegral (num `rem` ten) / fromIntegral (denom `quot` ten)) = (num, denom) : getCuriousFractions fracs
    | otherwise = getCuriousFractions fracs

multiplyFractions :: [(Int, Int)] -> (Int, Int)
multiplyFractions fracs = ((product $ map (fst) $ fracs), (product $ map (snd) $ fracs))

finalDenominator :: (Int, Int) -> Int
finalDenominator (num, denom) = denom `quot` (gcd num denom)