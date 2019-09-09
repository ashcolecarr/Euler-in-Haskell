-- Project Euler Problem 4
import Data.Time (getCurrentTime, diffUTCTime)

maxNumber = 999

main = do
    start <- getCurrentTime
    putStrLn $ show (largestPalindrome 0 maxNumber maxNumber)
    stop <- getCurrentTime
    putStrLn $ "Program execution took " ++ show (diffUTCTime stop start) ++ " seconds."

largestPalindrome :: Int -> Int -> Int -> Int
largestPalindrome final x y
    | x < 100 && y < 100 = final
    | show (x * y) == reverse (show (x * y)) = if final < x * y
                                               then largestPalindrome (x * y) x (y - 1)
                                               else largestPalindrome final x (y - 1)
    | otherwise = if y >= 100
                  then largestPalindrome final x (y - 1)
                  else largestPalindrome final (x - 1) maxNumber  