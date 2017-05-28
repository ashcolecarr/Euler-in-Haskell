-- Project Euler Problem 15

import Data.Time (getCurrentTime, diffUTCTime)

maxGridSize :: Integer
maxGridSize = 20

main = do
    start <- getCurrentTime
    putStr $ "The number of routes going only down or right in a " ++ show maxGridSize ++ " x " ++ show maxGridSize ++ " grid is "
    putStrLn $ show (combination maxGridSize) ++ "."
    stop <- getCurrentTime
    putStrLn $ "Program execution took " ++ show (diffUTCTime stop start) ++ " seconds."
    
-- Use the combination formula for the square grid
-- size! / (size / 2)! / (size / 2)!
combination :: Integer -> Integer
combination gridSize = (product [pathSize, pathSize - 1..(pathSize `div` 2) + 1]) `div` (product [1..gridSize]) 
  where
    pathSize = gridSize * 2