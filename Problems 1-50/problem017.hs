-- Project Euler Problem 17
import Data.Time (getCurrentTime, diffUTCTime)
import Data.Char (isAlpha)

main = do
    start <- getCurrentTime
    putStrLn $ show (numbersAsWords [1..1000])
    stop <- getCurrentTime
    putStrLn $ "Program execution took " ++ show (diffUTCTime stop start) ++ " seconds."

numbersAsWords :: [Int] -> Int
numbersAsWords xs = sum $ map (length . filter (isAlpha)) $ map (convertNumber) xs

convertNumber :: Int -> String
convertNumber 1000 = convertNumber (1000 `div` 1000) ++ " thousand"
convertNumber number
    | number > 99 = convertNumber (number `div` 100) ++ " hundred" ++ if (number `rem` 100 /= 0)
                                                                      then " and " ++ convertNumber (number `rem` 100)
                                                                      else ""
    | number > 19 = tenTeens (number `div` 10) ++ "ty" ++ if (number `rem` 10 /= 0)
                                                          then "-" ++ convertNumber (number `rem` 10)
                                                          else ""
    | number > 12 = tenTeens number ++ "teen"
    | otherwise = ones number
    where
        tenTeens :: Int -> String
        tenTeens n
            | n == 19 || n == 9 = "nine"
            | n == 18 || n == 8 = "eigh"
            | n == 17 || n == 7 = "seven"
            | n == 16 || n == 6 = "six"
            | n == 15 || n == 5 = "fif"
            | n == 14 = "four"
            | n == 4 = "for"
            | n == 13 || n == 3 = "thir"
            | n == 2 = "twen"
            | n == 1 = ""
        ones :: Int -> String
        ones n
            | n == 12 = "twelve"
            | n == 11 = "eleven"
            | n == 10 = "ten"
            | n == 9 = "nine"
            | n == 8 = "eight"
            | n == 7 = "seven"
            | n == 6 = "six"
            | n == 5 = "five"
            | n == 4 = "four"
            | n == 3 = "three"
            | n == 2 = "two"
            | n == 1 = "one"