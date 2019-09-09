-- Project Euler Problem 26
import Data.Time (getCurrentTime, diffUTCTime)
import Data.List (elemIndex)
import Data.Maybe (fromJust)

main = do
    start <- getCurrentTime
    putStrLn $ show (getLongest $ remainderList dList)
    stop <- getCurrentTime
    putStrLn $ "Program execution took " ++ show (diffUTCTime stop start) ++ " seconds."

maxNumber :: Int
maxNumber = 1000

-- List start at 2 and is zero indexed, so add 2 to result.
getLongest :: [[Int]] -> Int
getLongest rems = (fromJust ((maximum $ lengths rems) `elemIndex` lengths rems)) + 2
    where
        lengths :: [[Int]] -> [Int]
        lengths rems = map (length) rems

remainderList :: [Int] -> [[Int]]
remainderList nums = map (remainders 1 0 []) nums

-- Decimal places can go up to d - 1
remainders :: Int -> Int -> [Int] -> Int -> [Int]
remainders dividend count rems number
    | count == number = rems
    | remainder `elem` rems = rems
    | otherwise = remainders remainder (count + 1) (remainder : rems) number
    where
        remainder :: Int
        remainder = if dividend < number
                    then (dividend * 10) `rem` number
                    else dividend `rem` number

dList :: [Int]
dList = [2..(maxNumber - 1)]