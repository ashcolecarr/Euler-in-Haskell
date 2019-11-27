-- Project Euler Problem 46
import Data.Time (getCurrentTime, diffUTCTime)

main = do
    start <- getCurrentTime
    putStrLn $ show (getComposites oddComposites)
    stop <- getCurrentTime
    putStrLn $ "Program execution took " ++ show (diffUTCTime stop start) ++ " seconds."

getComposites :: [Int] -> Int
getComposites (x:xs)
    | checkValues x (squareTerms x) == True = x
    | otherwise = getComposites xs
    where
        checkValues :: Int -> [Int] -> Bool
        checkValues _ [] = True
        checkValues x (y:ys)
            | isPrime (x - y) = False
            | otherwise = checkValues x ys

oddComposites :: [Int]
oddComposites = filter (\x -> odd x && (not $ isPrime x)) [9..]

squareTerms :: Int -> [Int]
squareTerms max = [2 * (x ^ 2) | x <- [1..max], (2 * (x ^ 2)) < max]

isPrime :: Int -> Bool
isPrime n = n > 1 && foldr (\p r -> p * p > n || ((n `rem` p) /= 0 && r)) True primes

primes :: [Int]
primes = 2 : filter (isPrime) [3, 5..]
