-- Project Euler Problem 31

import Data.Time (getCurrentTime, diffUTCTime)

main = do
  start <- getCurrentTime
  putStr $ "Using any number of coins, the number of different ways \x00a3\x0032 can be made is "
  putStrLn $ show (countChange totalMoney (length coins)) ++ "."
  stop <- getCurrentTime
  putStrLn $ "Program execution took " ++ show (diffUTCTime stop start) ++ " seconds."

totalMoney :: Int
totalMoney = 200

coins :: [Int]
coins = [1, 2, 5, 10, 20, 50, 100, 200]

-- Coin change algorithm
countChange :: Int -> Int -> Int
countChange money change
  | money < 0 || change <= 0 = 0
  | money == 0 = 1
  | otherwise = (countChange money (change - 1)) + (countChange (money - coins !! (change - 1)) change)