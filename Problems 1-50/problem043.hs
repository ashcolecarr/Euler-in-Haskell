-- Project Euler Problem 43
import Data.Time (getCurrentTime, diffUTCTime)
import Data.List (permutations)

main = do
    start <- getCurrentTime
    putStrLn $ show (panDigitalSum)
    stop <- getCurrentTime
    putStrLn $ "Program execution took " ++ show (diffUTCTime stop start) ++ " seconds."

panDigitalSum :: Integer
panDigitalSum = sum $ map (read) $ filter (isDivisible 1 4 primes) allPandigitals

isDivisible :: Int -> Int -> [Int] -> String -> Bool
isDivisible _ _ [] _ = True
isDivisible start end primes number = if ((read $ slice number start end) `rem` (head primes)) == 0
                                      then isDivisible (start + 1) (end + 1) (tail primes) number
                                      else False

allPandigitals :: [String]
allPandigitals = filter (zeroIsNotFirstDigit) $ permutations number
    where
        number :: String
        number = "9876543210"
        zeroIsNotFirstDigit :: String -> Bool
        zeroIsNotFirstDigit digitString
            | head digitString == '0' = False
            | otherwise = True

primes :: [Int]
primes = [2, 3, 5, 7, 11, 13, 17]

slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice x start end = take (end - start) . drop start $ x