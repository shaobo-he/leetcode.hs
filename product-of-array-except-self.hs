module Main where

-- there's no so-called constant space complexity
productExceptSelf :: Num a => [a] -> [a]
productExceptSelf lst = let (_,_,r) = foldr combine (leftProds,rightProds,[]) lst in
  r where
    combine _ (lp,rp,res) = (tail lp, tail rp, (head lp)*(head rp):res)
    leftProds = foldl buildLeftProds [1] $ init lst where
      buildLeftProds lp@(f:r) e = f*e:lp
    rightProds = reverse $ foldr buildRightProds [1] $ tail lst where
      buildRightProds e rp@(f:r) = f*e:rp

main :: IO ()
main = do
  putStrLn $ show $ productExceptSelf [1,2,3,4]
  putStrLn $ show $ productExceptSelf [0,0]
  putStrLn $ show $ productExceptSelf [0,1]
  putStrLn $ show $ productExceptSelf [5,2,3]
