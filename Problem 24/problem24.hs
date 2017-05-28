-- Project Euler Problem 24

import Data.Time (getCurrentTime, diffUTCTime)
import Data.List (permutations, sort)

main = do
  start <- getCurrentTime
  putStr $ "The millionth lexicographic permutation of the digits is "
  putStrLn $ show (listPermutations) ++ "."
  stop <- getCurrentTime
  putStrLn $ "Program execution took " ++ show (diffUTCTime stop start) ++ " seconds."

listPermutations :: [Int]
listPermutations = (sort $ permutations [0..maxNum]) !! chosen

chosen :: Int
chosen = 999999

maxNum :: Int
maxNum = 9