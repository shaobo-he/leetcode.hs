module Main

maxProfit : List Double -> Double
maxProfit [] = 0
maxProfit (price :: prices) = maxProfitHelper prices price 0 where
  maxProfitHelper : List Double -> Double -> Double -> Double
  maxProfitHelper [] lowestPrice currMaxProfit = currMaxProfit
  maxProfitHelper (price :: prices) lowestPrice currMaxProfit = if (price < lowestPrice)
      then maxProfitHelper prices price currMaxProfit
      else maxProfitHelper prices lowestPrice (price - lowestPrice `max` currMaxProfit)


main : IO ()
main = do
  putStrLn (show $ maxProfit [7,1,5,3,6,4])
  putStrLn (show $ maxProfit [7,6,4,3,1])
