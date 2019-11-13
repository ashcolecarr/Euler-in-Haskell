-- Project Euler Problem 43
import Data.Time (getCurrentTime, diffUTCTime)
import Data.List (permutations)

main = do
    start <- getCurrentTime
    putStrLn $ show ()
    stop <- getCurrentTime
    putStrLn $ "Program execution took " ++ show (diffUTCTime stop start) ++ " seconds."

allPandigitals :: [String]
allPandigitals = map (zeroIsNotFirst) permutations number
    where
        number :: String
        number = "98765462310"
        zeroIsNotFirstDigit :: String -> Bool
        zeroIsNotFirstDigit digitString
            | head digitString == "0" = False
            | otherwise = True