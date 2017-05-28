-- Project Euler Problem 29

import Data.Time (getCurrentTime, diffUTCTime)
import Data.List (nub)

main = do
  start <- getCurrentTime
  putStr $ "The number of distinct terms in the sequence generated from a ^ b where 2 <= a <= " ++ show maxNumber ++ " and 2 <= b <= " ++ show maxNumber ++ " is "
  putStrLn $ show (length $ nub $ powerSequence) ++ "."
  stop <- getCurrentTime
  putStrLn $ "Program execution took " ++ show (diffUTCTime stop start) ++ " seconds."

maxNumber :: Integer
maxNumber = 100

powerSequence :: [Integer]
powerSequence = [a ^ b | a <- [2..maxNumber], b <- [2..maxNumber]]