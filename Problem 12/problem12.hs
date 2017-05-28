-- Project Euler Problem 12

import Data.Time (getCurrentTime, diffUTCTime)
import Data.List (group)

divisors :: Int
divisors = 500

main = do
    start <- getCurrentTime
    putStr $ "The first triangular number with over " ++ show divisors ++ " divisors is "
    putStrLn $ show (getNumber $ triangularNumbers [1..]) ++ "."
    stop <- getCurrentTime
    putStrLn $ "Program execution took " ++ show (diffUTCTime stop start) ++ " seconds."
    
getNumber :: [Int] -> Int    
getNumber (n:ns)
  | product (map (\n -> length n + 1) (group $ factors n)) > divisors = n
  | otherwise = getNumber ns

triangularNumbers :: [Int] -> [Int]
triangularNumbers ns = map (\n -> (n * (n + 1)) `div` 2) ns

factors :: Int -> [Int]
factors m = factor m (head primes) (tail primes) 
  where
    factor m n ns
      | m < 2 = []
      | m < n ^ 2 = [m]
      | m `mod` n == 0 = n : factor (m `div` n) n ns
      | otherwise = factor m (head ns) (tail ns)
                    
primes :: [Int]
primes = 2 : filter (\n-> head (factors n) == n) [3,5..]