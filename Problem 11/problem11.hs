-- Project Euler Problem 11

import Data.Time (getCurrentTime, diffUTCTime)
import Data.List (zipWith4)

adjacents :: Int
adjacents = 4

main = do
    start <- getCurrentTime
    let grid = [[8, 2, 22, 97, 38, 15, 0, 40, 0, 75, 4, 5, 7, 78, 52, 12, 50, 77, 91, 8], 
                [49, 49, 99, 40, 17, 81, 18, 57, 60, 87, 17, 40, 98, 43, 69, 48, 4, 56, 62, 0], 
                [81, 49, 31, 73, 55, 79, 14, 29, 93, 71, 40, 67, 53, 88, 30, 3, 49, 13, 36, 65], 
                [52, 70, 95, 23, 4, 60, 11, 42, 69, 24, 68, 56, 1, 32, 56, 71, 37, 2, 36, 91], 
                [22, 31, 16, 71, 51, 67, 63, 89, 41, 92, 36, 54, 22, 40, 40, 28, 66, 33, 13, 80], 
                [24, 47, 32, 60, 99, 3, 45, 2, 44, 75, 33, 53, 78, 36, 84, 20, 35, 17, 12, 50],
                [32, 98, 81, 28, 64, 23, 67, 10, 26, 38, 40, 67, 59, 54, 70, 66, 18, 38, 64, 70],
                [67, 26, 20, 68, 2, 62, 12, 20, 95, 63, 94, 39, 63, 8, 40, 91, 66, 49, 94, 21],
                [24, 55, 58, 5, 66, 73, 99, 26, 97, 17, 78, 78, 96, 83, 14, 88, 34, 89, 63, 72],
                [21, 36, 23, 9, 75, 0, 76, 44, 20, 45, 35, 14, 0, 61, 33, 97, 34, 31, 33, 95],
                [78, 17, 53, 28, 22, 75, 31, 67, 15, 94, 03, 80, 4, 62, 16, 14, 9, 53, 56, 92],
                [16, 39, 5, 42, 96, 35, 31, 47, 55, 58, 88, 24, 0, 17, 54, 24, 36, 29, 85, 57],
                [86, 56, 0, 48, 35, 71, 89, 7, 5, 44, 44, 37, 44, 60, 21, 58, 51, 54, 17, 58],
                [19, 80, 81, 68, 5, 94, 47, 69, 28, 73, 92, 13, 86, 52, 17, 77, 4, 89, 55, 40],
                [4, 52, 8, 83, 97, 35, 99, 16, 7, 97, 57, 32, 16, 26, 26, 79, 33, 27, 98, 66],
                [88, 36, 68, 87, 57, 62, 20, 72, 3, 46, 33, 67, 46, 55, 12, 32, 63, 93, 53, 69],
                [4, 42, 16, 73, 38, 25, 39, 11, 24, 94, 72, 18, 8, 46, 29, 32, 40, 62, 76, 36],
                [20, 69, 36, 41, 72, 30, 23, 88, 34, 62, 99, 69, 82, 67, 59, 85, 74, 4, 36, 16],
                [20, 73, 35, 29, 78, 31, 90, 1, 74, 31, 49, 71, 48, 86, 81, 16, 23, 57, 5, 54],
                [1, 70, 54, 71, 83, 51, 54, 69, 16, 92, 33, 48, 61, 43, 52, 1, 89, 19, 67, 48]]
    
    putStr $ "The greatest product of four adjacent numbers in the 20 x 20 grid is "
    putStrLn $ show (gridProduct grid) ++ "."
    stop <- getCurrentTime
    putStrLn $ "Program execution took " ++ show (diffUTCTime stop start) ++ " seconds."
    
gridProduct :: [[Int]] -> Int
gridProduct [[]] = 0
gridProduct grid = maximum ((horizontal grid 0) : (vertical grid 0) : (diagonalAscending grid 0) : (diagonalDescending grid 0) : [])

horizontal :: [[Int]] -> Int -> Int
horizontal [] largest = largest
horizontal (x:xs) largest
    | hScan x 0 > largest = horizontal xs (hScan x 0)
    | otherwise = horizontal xs largest
    where
        hScan :: [Int] -> Int -> Int
        hScan [] _ = 0
        hScan ys largest'
            | (length $ take adjacents ys) < adjacents = largest'
            | candidate > largest' = hScan (tail ys) candidate
            | otherwise = hScan (tail ys) largest'
            where
                candidate = product $ take adjacents ys

vertical :: [[Int]] -> Int -> Int
vertical [] largest = largest
vertical xs largest
    | (length $ take adjacents xs) < adjacents = largest
    | candidate > largest = vertical (tail xs) candidate
    | otherwise = vertical (tail xs) largest
    where
        candidate = maximum $ zipWith4 (\a b c d -> a * b * c * d) (xs !! 0) (xs !! 1) (xs !! 2) (xs !! 3)
        
diagonalAscending :: [[Int]] -> Int -> Int
diagonalAscending [] largest = largest
diagonalAscending xs largest
    | (length $ take adjacents xs) < adjacents = largest
    | candidate > largest = diagonalAscending (tail xs) candidate
    | otherwise = diagonalAscending (tail xs) largest
    where
        candidate = maximum $ zipWith4 (\a b c d -> a * b * c * d) (tail $ tail $ tail (xs !! 0)) (tail $ tail (xs !! 1)) (tail (xs !! 2)) (xs !! 3)

diagonalDescending :: [[Int]] -> Int -> Int
diagonalDescending [] largest = largest
diagonalDescending xs largest
    | (length $ take adjacents xs) < adjacents = largest
    | candidate > largest = diagonalDescending (tail xs) candidate
    | otherwise = diagonalDescending (tail xs) largest
    where
        candidate = maximum $ zipWith4 (\a b c d -> a * b * c * d) (xs !! 0) (tail (xs !! 1)) (tail $ tail (xs !! 2)) (tail $ tail $ tail (xs !! 3))
