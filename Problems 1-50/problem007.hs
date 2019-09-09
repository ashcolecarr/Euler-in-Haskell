-- Project Euler Problem 7
import Data.Time (getCurrentTime, diffUTCTime)

maxNumber :: Int
maxNumber = 10000

main = do
    start <- getCurrentTime
    putStrLn $ show (getPrime (primeCandidates [6, 12..]) 2)
    stop <- getCurrentTime
    putStrLn $ "Program execution took " ++ show (diffUTCTime stop start) ++ " seconds."

-- Tally the primes from the list of prime candidates using trial division.
getPrime :: [Int] -> Int -> Int
getPrime [] _ = 0
getPrime (x:xs) y = if trialDivide x 3
                    then if y == maxNumber
                         then x
                         else getPrime xs (y + 1)
                    else getPrime xs y
    where
        trialDivide :: Int -> Int -> Bool
        trialDivide z acc = if acc <= (floor $ sqrt $ fromIntegral z)
                            then if z `mod` acc == 0
                                 then False
                                 else trialDivide z (acc + 2)
                            else True

-- Make a list of prime candidates using the fact that primes are in the form 6k +/- 1
primeCandidates :: [Int] -> [Int]
primeCandidates [] = []
primeCandidates (x:xs) = (x - 1) : (x + 1) : primeCandidates xs