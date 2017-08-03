module Main where

import Annuity -- (presentValue, futureValue, perpetuity)


-- from wikipedia https://en.wikipedia.org/wiki/Mortgage_calculator
-- mortgage principal P, coupon c, term n:  c = rP / [1 - (1 + r)^(-n)]
-- define mortgage Yield = c/P = y, then:
-- mortgage yield = c/P = y = r / (1 - 1/(1+r)^N) = r / (1 - presentValue)

mortgageYield r n = r / (1 - presentValue r n)

-- y / r = 1 / (1 - presentValue) = futureValue /( futureValue - 1)
-- this implies: futureValue = y / (y - r)
-- log futureValue = log y - log (y - r)
-- but also log futureValue = n log (1 + r), so
-- n = (log y - log (y - r)) / log (1 + r)

mortgageTerm r y = (log y - log (y - r)) / log (1 + r)





futureValueMultiplier :: (Fractional a, Integral b) => a -> b -> a
futureValueMultiplier rate years = (1 + rate/100) ^ years -- rate is a percentage



yieldFromQuoted principal quote = 100 * (quote * 12) / principal

futureValueAdjustedYield yieldPercentage principal years quote = yfq / fvm where
  yfq = yieldFromQuoted principal quote
  fvm = futureValueMultiplier yieldPercentage years




-- define mortgage coupon rate R = coupon / principal = c/P
-- then:  R/r = (1+r)^N / [(1+r)^N - 1]
-- and: (1+r)^N = R / (R- r)
-- finally: log N = log R - log (R-r) / log (1+r)

-- couponRate r n = r*s/(s - 1) where s = (1 + r/100)^n
-- yieldOverInterest r n = s / (s - 1) where s = (1 + r/100)^n
-- rateOverYield r n = (s - 1) / s where s = (1 + r/100)^n

implicitN r cR = exp logN where
  logN = (log cR - log (cR - r)) / log (1 + r)

expectedN yieldPercentage principal years quote = implicitN yieldPercentage fvy where
  fvy = futureValueAdjustedYield yieldPercentage principal years quote


fvm = futureValueMultiplier 2.85
yfq = yieldFromQuoted 100000


main :: IO ()
main = do
  let
    r = 2.85
    p = 100000
    n = 22
    q = 1238
  putStrLn $ "Expected N: " ++ show (expectedN r p n q)
  putStrLn $ "Future adjusted yield: " ++ show(futureValueAdjustedYield r p n q)
  putStrLn $ "yield from quoted: " ++ show (yfq 1238) ++ "%"
  putStrLn $ "future value mult: " ++ show (fvm 22)
  putStrLn $ "adjusted fv yield: " ++ show (yfq 1238 / fvm 22) ++ "%"
  putStr "fvm: "
  print $ fvm 20
  putStrLn $ "fvm 0: " ++ (show .fvm) 0
  putStrLn $ "fvm 1: " ++ show (fvm 1)
