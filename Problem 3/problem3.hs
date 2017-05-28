-- Project Euler Problem 3
import Data.Time (getCurrentTime, diffUTCTime)

factoredNumber = 600851475143
two = 2

main = do
    start <- getCurrentTime
    
    putStrLn $ "The largest prime factor of " ++ show factoredNumber ++ " is " ++ show (largestPrimeFactor factoredNumber two) ++ "."
    stop <- getCurrentTime
    putStrLn $ "Program execution took " ++ show (diffUTCTime stop start) ++ " seconds."

largestPrimeFactor :: Int -> Int -> Int
largestPrimeFactor number factor = if number `mod` factor == 0
                                   then largestPrimeFactor (number `div` factor) 2
                                   else if factor > number `div` 2  
                                        then number
                                        else if factor == 2     
                                             then largestPrimeFactor number (factor + 1)   
                                             else largestPrimeFactor number (factor + 2)