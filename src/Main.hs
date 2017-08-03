module Main where
import Annuity (numberOfPayments, futureValue)

quotesFromSite :: IO [(Integer, Float)]
quotesFromSite = do
  quotesString <- readFile "data/immediateannuitiesquotes.txt"
  let
    readLine line = (read i :: Integer, read j :: Float) where [i,j] = words line
  return $ map readLine (lines quotesString)

processQuote :: Float -> Integer -> Float -> IO ()
processQuote principal years quotePerMonth = do
  let
    age = 43
    rate30YearTreasury = 2.85
    r = rate30YearTreasury / 100
    fv = futureValue r years
    futurePrincipal = principal * fv
    cr = 12 * quotePerMonth / futurePrincipal
    impliedTime = numberOfPayments r cr
    breakEvenDeathAge = age + years + round impliedTime
  putStrLn ""
  print years
  putStrLn $ "starting age: " ++ show (age + years)
  putStrLn $ "coupon rate: " ++ show (cr * 100) ++ "%"
  putStrLn $ "implied time in quote: " ++ show impliedTime
  putStrLn $ "break even death age: " ++ show breakEvenDeathAge


main :: IO ()
main = do
  let
    principal = 100000
    f (year, quotePerMonth) = processQuote principal year quotePerMonth
  putStrLn $ "Running immediateannuities.com with principal = " ++ show principal
  quotes <- quotesFromSite
  mapM_ f quotes
