module IOFunctor where

-- start snippet io-functor
randomDouble :: IO Double -- between 0.0 and 1.0

randomLotteryPrize :: IO Integer
randomLotteryPrize =
  fmap toAmount randomDouble
  where
    toAmount d = round (d * 1000000)
-- end snippet io-functor

randomDouble = return 0.561231239
