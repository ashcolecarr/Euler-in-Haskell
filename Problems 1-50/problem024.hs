-- Project Euler Problem 24
import Data.Time (getCurrentTime, diffUTCTime)
import Data.List (permutations, sort)

main = do
    start <- getCurrentTime
    putStrLn $ show (getPermutation)
    stop <- getCurrentTime
    putStrLn $ "Program execution took " ++ show (diffUTCTime stop start) ++ " seconds."

getPermutation :: [Char]
getPermutation = concatMap (show) $ (sort $ permutations [0..maxNumber]) !! chosen

chosen :: Int
chosen = 999999

maxNumber :: Int
maxNumber = 9