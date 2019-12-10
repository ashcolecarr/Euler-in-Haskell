-- Project Euler Problem 47
import Data.Time (getCurrentTime, diffUTCTime)
import Data.Set (fromList)

main = do
    start <- getCurrentTime
    putStrLn $ show (checkFactors numbers)
    stop <- getCurrentTime
    putStrLn $ "Program execution took " ++ show (diffUTCTime stop start) ++ " seconds."

checkFactors :: [Int] -> Int
checkFactors (x:xs) = if getDistinctFactors x 4 == True
                      then x
                      else checkFactors xs

getDistinctFactors :: Int -> Int -> Bool
getDistinctFactors _ 0 = True 
getDistinctFactors x count 
    | (length . fromList . factors) x == 4 = getDistinctFactors (x + 1) (count - 1)
    | otherwise = False

-- Start at 210, since it is 2 x 3 x 5 x 7
numbers :: [Int]
numbers = [210..]

factors :: Int -> [Int]
factors m = factor m (head primes) (tail primes)
    where
        factor :: Int -> Int -> [Int] -> [Int]
        factor m n ns
            | m < 2 = []
            | m < n ^ 2 = [m]
            | m `rem` n == 0 = n : factor (m `quot` n) n ns
            | otherwise = factor m (head ns) (tail ns)

primes :: [Int]
primes = 2 : filter (\n -> head (factors n) == n) [3, 5..]