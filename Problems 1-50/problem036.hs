-- Project Euler Problem 36
import Data.Time (getCurrentTime, diffUTCTime)
import Data.Char (intToDigit)

main = do
    start <- getCurrentTime
    putStrLn $ show (getPalindromeSum)
    stop <- getCurrentTime
    putStrLn $ "Program execution took " ++ show (diffUTCTime stop start) ++ " seconds."

getPalindromeSum :: Int
getPalindromeSum = sum (filter (isPalindrome) numbers)
    where
        numbers :: [Int]
        numbers = [1..999999]

isPalindrome :: Int -> Bool
isPalindrome num = show num == (reverse . show) num && convertIntToBinary num == (reverse . convertIntToBinary) num

convertIntToBinary :: Int -> String
convertIntToBinary 0 = "0"
convertIntToBinary num = (reverse . divideByTwo) num
    where
        divideByTwo :: Int -> String
        divideByTwo num
            | num == 0 = ""
            | otherwise = (intToDigit $ num `rem` 2) : divideByTwo (num `quot` 2)