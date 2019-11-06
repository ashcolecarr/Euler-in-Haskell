-- Project Euler Problem 42
import Data.Time (getCurrentTime, diffUTCTime)
import System.IO (readFile)
import Data.Map (Map, fromList, lookup)
import Data.Maybe (fromMaybe)

main = do
    start <- getCurrentTime
    wordList <- readFile "words.txt"
    putStrLn $ show (countTriangleWords wordList)
    stop <- getCurrentTime
    putStrLn $ "Program execution took " ++ show (diffUTCTime stop start) ++ " seconds."

countTriangleWords :: String -> Int
countTriangleWords wordList = length $ filter (isTriangleNumber 1) $ map (getWordScore) $ words $ map (markWordBoundaries) wordList

isTriangleNumber :: Int -> Int -> Bool
isTriangleNumber x candidate 
    | triangleFormula x < candidate = isTriangleNumber (x + 1) candidate
    | triangleFormula x > candidate = False
    | triangleFormula x == candidate = True
    where
        triangleFormula :: Int -> Int
        triangleFormula n = (n * (n + 1)) `quot` 2

markWordBoundaries :: Char -> Char
markWordBoundaries x
    | x == '\"' || x == ',' = ' '
    | otherwise = x

getWordScore :: String -> Int
getWordScore "" = 0
getWordScore (x:xs) = (fromMaybe 0 $ Data.Map.lookup x letterScore) + getWordScore xs

-- Dictionary of letters
letterScore :: Map Char Int
letterScore = fromList [('A', 1), ('B', 2), ('C', 3), ('D', 4), ('E', 5), ('F', 6), ('G', 7), ('H', 8), ('I', 9), ('J', 10), 
                        ('K', 11), ('L', 12), ('M', 13), ('N', 14), ('O', 15), ('P', 16), ('Q', 17), ('R', 18), ('S', 19), ('T', 20), 
                        ('U', 21), ('V', 22), ('W', 23), ('X', 24), ('Y', 25), ('Z', 26)]