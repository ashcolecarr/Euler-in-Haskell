-- Project Euler Problem 38
import Data.Time (getCurrentTime, diffUTCTime)
import Data.Set (fromList)

main = do
    start <- getCurrentTime
    putStrLn $ show (largestPandigital)
    stop <- getCurrentTime
    putStrLn $ "Program execution took " ++ show (diffUTCTime stop start) ++ " seconds."

{- 
    The problem already gives the pandigital number 9 x 1, 2, 3, 4, 5, so the final
    answer must also start with a 9 and be at least as large as (9 x 1,2,3,4,5).
-}
multiples :: [Int]
multiples = [read $ show x ++ show (x * 2) | x <- [lowerBound..upperBound]]
    where
        lowerBound :: Int
        lowerBound = 9123
        upperBound :: Int
        upperBound = 9876

largestPandigital :: Int
largestPandigital = maximum [x | x <- multiples, '0' `notElem` show x && (length . fromList . show $ x) == 9]