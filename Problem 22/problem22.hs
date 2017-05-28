-- Project Euler Problem 22

import Data.Time (getCurrentTime, diffUTCTime)
import System.IO (readFile)
import Data.List (sort, foldl')

main = do
  start <- getCurrentTime
  names <- readFile "names.txt"
  putStr $ "The total of all the name scores in names.txt is "
  putStrLn $ show (nameScore (sort $ words $ map (splitNames) names) 1) ++ "."
  stop <- getCurrentTime
  putStrLn $ "Program execution took " ++ show (diffUTCTime stop start) ++ " seconds."

splitNames :: Char -> Char
splitNames x
  | x == '\"' || x == ',' = ' '
  | otherwise = x
                
nameScore :: [String] -> Integer -> Integer
nameScore [] _ = 0
nameScore (name:names) position = ((foldl' score 0 name) * position) + (nameScore names (position + 1))
  where
    score :: Integer -> Char -> Integer
    score acc x
      | x == 'A' = acc + 1
      | x == 'B' = acc + 2
      | x == 'C' = acc + 3      
      | x == 'D' = acc + 4      
      | x == 'E' = acc + 5      
      | x == 'F' = acc + 6      
      | x == 'G' = acc + 7      
      | x == 'H' = acc + 8      
      | x == 'I' = acc + 9      
      | x == 'J' = acc + 10      
      | x == 'K' = acc + 11     
      | x == 'L' = acc + 12     
      | x == 'M' = acc + 13     
      | x == 'N' = acc + 14     
      | x == 'O' = acc + 15     
      | x == 'P' = acc + 16     
      | x == 'Q' = acc + 17     
      | x == 'R' = acc + 18     
      | x == 'S' = acc + 19     
      | x == 'T' = acc + 20     
      | x == 'U' = acc + 21     
      | x == 'V' = acc + 22     
      | x == 'W' = acc + 23     
      | x == 'X' = acc + 24     
      | x == 'Y' = acc + 25     
      | x == 'Z' = acc + 26
      | otherwise = acc + 0