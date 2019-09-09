-- Project Euler Problem 32
import Data.Time (getCurrentTime, diffUTCTime)
import Data.List (permutations)

main = do
    start <- getCurrentTime
    putStrLn $ show (pandigits permutationList [] 0)
    stop <- getCurrentTime
    putStrLn $ "Program execution took " ++ show (diffUTCTime stop start) ++ " seconds."

{- To get the pandigital digits needed, the multiplicand and multiplier need to
   be a certain number of digits, otherwise will have too many or too few digits.
-}
digits :: [Int]
digits = [1..9]

permutationList :: [[Int]]
permutationList = permutations digits

multiplicand :: [Int] -> Int -> Int
multiplicand xs y = fromDigits $ take y xs

multiplier :: [Int] -> Int -> Int -> Int
multiplier xs y z = fromDigits $ take y $ drop z xs

product' :: [Int] -> Int
product' xs = fromDigits $ drop 5 xs

pandigits :: [[Int]] -> [Int] -> Int -> Int
pandigits [] _ total = total
pandigits (x:xs) products total
    | multiplicand x 1 * multiplier x 4 1 == product' x && not ((product' x) `elem` products) = pandigits xs ((product' x) : products) (total + (product' x)) 
    | multiplicand x 2 * multiplier x 3 2 == product' x && not ((product' x) `elem` products) = pandigits xs ((product' x) : products) (total + (product' x))
    | otherwise = pandigits xs products total

fromDigits :: [Int] -> Int
fromDigits = foldl addDigit 0
    where
        addDigit :: Int -> Int -> Int
        addDigit number digit = 10 * number + digit