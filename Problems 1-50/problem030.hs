-- Project Euler Problem 30
import Data.Time (getCurrentTime, diffUTCTime)

main = do
    start <- getCurrentTime
    putStrLn $ show (sumDigits 2 0)
    stop <- getCurrentTime
    putStrLn $ "Program execution took " ++ show (diffUTCTime stop start) ++ " seconds."

power :: Int
power = 5

-- The possible upper bound of values is six digits long (9 ^ 5) * 6 since (9 ^ 5) * 7 is 413343,
-- which is less than seven digits long.
upperBound :: Int
upperBound = 354294

sumDigits :: Int -> Int -> Int
sumDigits current total = if current >= upperBound
                          then total
                          else if current == sum (map (^ power) (splitNumber current))
                               then sumDigits (current + 1) (total + current)
                               else sumDigits (current + 1) total

splitNumber :: Int -> [Int]
splitNumber 0 = []
splitNumber number = (number `rem` 10) : splitNumber (number `quot` 10)