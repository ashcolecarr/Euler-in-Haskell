-- Project Euler Problem 51
import Data.Time (getCurrentTime, diffUTCTime)

{- Notes and Assumptions:
 The first replaced digit must be no greater than 2, to get eight primes.
 The number of digits replaced will be three.
 Assume that the number will be five or six digits.
 The ones digits will never change.
 The prime can only end in 1, 3, 7, or 9. -}
main = do
    start <- getCurrentTime
    putStrLn $ show ()
    stop <- getCurrentTime
    putStrLn $ "Program execution took " ++ show (diffUTCTime stop start) ++ " seconds."



isPrime :: Int -> Bool
isPrime n = n > 1 && foldr (\p r -> p * p > n || ((n `rem` p) /= 0 && r)) True primes

primes :: [Int]
primes = 2 : filter (isPrime) [3, 5..maxNumber]
    where
        maxNumber :: Int
        maxNumber = 9999
