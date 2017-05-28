-- Project Euler Problem 19: How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?

import Data.Time (getCurrentTime, diffUTCTime)

startYear :: Int
startYear = 1901

main = do
  start <- getCurrentTime
  putStr $ "The number of Sundays falling on the first of the month in the twentieth century is "
  putStrLn $ show (addSundays 6 startYear) ++ "."
  stop <- getCurrentTime
  putStrLn $ "Program execution took " ++ show (diffUTCTime stop start) ++ " seconds."
  
-- 1 January 1901 was a Tuesday and the first Sunday was 6 January, 1901.
addSundays :: Int -> Int -> Int
addSundays day currentYear
  | currentYear > maxYear = 0
  | day > year = addSundays (day - year) (currentYear + 1)
  | day == january || day == february || day == march || day == april || day == may || day == june || day == july || day == august || day == september || day == october || day == november || day == december = 1 + addSundays (day + week) currentYear
  | otherwise = addSundays (day + week) currentYear
  where
    january = 1
    february = january + 31
    march = if currentYear `rem` 4 == 0 then february + 29 else february + 28
    april = march + 31
    may = april + 30
    june = may + 31
    july = june + 30
    august = july + 31
    september = august + 31
    october = september + 30
    november = october + 31
    december = november + 30
    year = if currentYear `rem` 4 == 0 then 366 else 365
    week = 7
    maxYear = 2000