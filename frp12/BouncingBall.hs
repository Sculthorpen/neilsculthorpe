-- The Classic "Bouncing Ball" in Yampa

{-# LANGUAGE Arrows #-}

module BouncingBall where

import FRP.Yampa

type Acceleration = Double
type Velocity = Double
type Height = Double

type Ball = (Height,Velocity)

g :: Acceleration
g = 9.81

iIntegral :: Double -> SF Double Double 
iIntegral x = integral >>> arr (+x)

fallingBall :: Ball -> SF () Ball
fallingBall (h0,v0) = proc _ -> do
                        v <- iIntegral v0  -< -g
                        h <- iIntegral h0  -< v
                        returnA            -< (h,v)

detectImpact :: SF Ball (Event Ball)
detectImpact =  proc (h,v) -> do
                  e <- edge  -<  (h <= 0)
                  returnA    -<  tag e (h,-v)

bouncingBall :: Ball -> SF () Ball
bouncingBall b = switch (fallingBall b >>> (returnA &&& detectImpact)) bouncingBall


-- reactimate :: IO a -> (Bool -> IO (DTime, Maybe a)) -> (Bool -> b -> IO Bool) -> SF a b -> IO ()

-- embed :: SF a b -> (a, [(DTime, Maybe a)]) -> [b]
-- deltaEncode :: Eq a => DTime -> [a] -> (a, [(DTime, Maybe a)])

main :: IO ()
main =  mapM_ (putStrLn.show) (runBB 25 0.1 10)

runBB :: Int -> DTime -> Height -> [Height]
runBB n dt h =  map (sig 2 .fst) (embed (bouncingBall (h,0)) (deltaEncode dt (replicate n ())))

sig :: RealFrac a => Int -> a -> a
sig n = (/m).fromIntegral.round.(*m)
        where m = 10 ^ n
              
