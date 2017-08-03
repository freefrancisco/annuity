module Annuity
-- (presentValue
-- , futureValue
-- , perpetuity
-- )
where

-- future value of one dollar
futureValue :: (Num a, Integral b) => a -> b -> a
futureValue r n = (1 + r)^n

-- present value of a dollar in the future
presentValue :: (Fractional a, Integral b) => a -> b -> a
presentValue r n = 1 / futureValue r n

-- perpetuity pays one dollar forever, how much is it worth?
-- the same amount of money I would need to have to produce one dollar in interest forever
--  so perpetuity * r = 1, which means perpetuity = 1/r
perpetuity :: (Fractional a) => a -> a
perpetuity r = 1 / r

-- annuity pays one dollar for n years, how much is it worth?
-- same as perpetuity that stops paying after n years, so perpetuity now minus perpetuity after n years
-- annuity = perp now - perp later = perp - present value of perp = perp (1 - pv)
annuity :: (Fractional a, Integral b) => a -> b -> a
annuity r n = perpetuity r * (1 - presentValue r n)

-- fv of perpetuity = perp * fv
annuityFV :: (Fractional a, Integral b) => a -> b -> a
annuityFV r n = annuity r n * futureValue r n

-- less efficient but equivalent implementations
annuity' :: (Fractional a, Integral b) => a -> b -> a
annuityFV' :: (Fractional a, Integral b) => a -> b -> a
annuity' r n = sum $ map (presentValue r) [1..n]
annuityFV' r n = sum $ map (futureValue r) [0..n-1]

-- the coupon rate of an annuity is what it pays divided by its face value,
-- in our case it pays one dollar, and the face value is the present value of the annuity
couponRate :: (Fractional a, Integral b) => a -> b -> a
couponRate r n = 1 / annuity r n

-- to back up the implicit n from a given coupon rate cr we solve for n:
-- cr = 1/annuity => 1/cr = annuity = perpetuity * (1 - pv) = 1/r * (1 - pv)
-- 1/cr = 1/r * (1 - pv) => r/cr = 1 - pv => pv = 1 - r/cr, but pv = (1 + r)^(-n), so
-- pv = (1+r)^(-n) = 1 - r/cr = (cr - r)/cr, taking the logs:
-- (-n)*log(1+r) = log (cr - r) / cr = -log (cr / (cr - r))
-- n = log(cr / (cr - r)) / log(1 + r) = (log cr - log (cr - r)) / log (1 + r)
numberOfPayments :: Floating a => a -> a -> a
numberOfPayments r cr = (log cr - log (cr - r)) / log (1 + r)
