-- Project Euler Problem 39
import Data.Time (getCurrentTime, diffUTCTime)
import Data.List (group, sort, maximumBy)
import Data.Function (on)

main = do
    start <- getCurrentTime
    putStrLn $ show (maximumSolutions)
    stop <- getCurrentTime
    putStrLn $ "Program execution took " ++ show (diffUTCTime stop start) ++ " seconds."

triangleSolutions :: [(Int, Int, Int, Int)]
triangleSolutions = [(p, a, b p a, c p a) | p <- [2, 4..maxNum], a <- [1..(maxA p)], a ^ 2 + (b p a) ^ 2 == (c p a) ^ 2]
    where
        b :: Int -> Int -> Int
        b p a = (p ^ 2 - 2 * p * a) `quot` (2 * p - 2 * a)
        c :: Int -> Int -> Int
        c p a = p - a - (b p a)
        maxNum :: Int
        maxNum = 1000
        maxA :: Int -> Int
        maxA p = p `quot` 3 + 1

perimeters :: [Int]
perimeters = [getPerimeter x | x <- triangleSolutions]
    where
        getPerimeter :: (Int, Int, Int, Int) -> Int
        getPerimeter (x, _, _, _) = x

maximumSolutions :: Int
maximumSolutions = fst $ maximumBy (compare `on` snd) $ map (solutions) $ (group . sort) perimeters
    where
        solutions :: [Int] -> (Int, Int)
        solutions perimeterGroup = (head perimeterGroup, length perimeterGroup)