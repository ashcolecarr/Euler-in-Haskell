-- Project Euler Problem 49
import Data.Time (getCurrentTime, diffUTCTime)
import Data.List (permutations)

main = do
    start <- getCurrentTime
    putStrLn $ show (getTerm validPrimes)
    stop <- getCurrentTime
    putStrLn $ "Program execution took " ++ show (diffUTCTime stop start) ++ " seconds."

getTerm :: [Int] -> String
getTerm (x:xs) 
    | (checkPrimes x $ primePermutations x) == "" = getTerm xs
    | otherwise = checkPrimes x $ primePermutations x
    where
        checkPrimes :: Int -> [Int] -> String
        checkPrimes _ [] = ""
        checkPrimes x (y:ys)
            | x == y = checkPrimes x ys -- Skip if it's the same number.
            | y `elem` validPrimes = if (y + (y - x)) `elem` primePermutations x
                                     then show x ++ show y ++ show (y + (y - x))
                                     else checkPrimes x ys
            | otherwise = checkPrimes x ys
        primePermutations :: Int -> [Int]
        primePermutations x = filter (isPrime) $ map (read::String->Int) $ permutations $ show x

validPrimes :: [Int]
validPrimes = filter (`notElem` excludedDigits) [x | x <- primes, x > givenDigit]

-- The problem already gives these, so they are screened out.
excludedDigits :: [Int]
excludedDigits = map (read::String->Int) $ permutations $ show givenDigit

givenDigit :: Int
givenDigit = 1487

isPrime :: Int -> Bool
isPrime n = n > 1 && foldr (\p r -> p * p > n || ((n `rem` p) /= 0 && r)) True primes

primes :: [Int]
primes = 2 : filter (isPrime) [3, 5..maxNumber]
    where
        maxNumber :: Int
        maxNumber = 9999

