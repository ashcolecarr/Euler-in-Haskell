-- Project Euler Problem 1
import Data.Time (getCurrentTime, diffUTCTime)

three = 3
five = 5
thousand = 1000

main = do
    start <- getCurrentTime
    
    putStrLn $ "The sum of all the multiples of " ++ show three ++ " or " ++ show five ++ " below " ++ show thousand ++ " is " ++ show (multiplesOfThreeOrFiveSum [1..(thousand - 1)]) ++ "."
    stop <- getCurrentTime
    putStrLn $ "Program execution took " ++ show (diffUTCTime stop start) ++ " seconds."
    
multiplesOfThreeOrFiveSum :: [Int] -> Int
multiplesOfThreeOrFiveSum xs = sum [x | x <- xs, x `mod` three == 0 || x `mod` five == 0 ]