module Main where

import System.Random
import System.ProgressBar
import Control.Concurrent (threadDelay)
import Data.Foldable (for_)
import qualified System.Console.Pretty as P

-- https://www.wikiwand.com/en/Beta_distribution
data BetaDist = BetaDist { alpha :: Int, beta :: Int } deriving (Show)
data Color = Yellow | Red | Blue deriving (Show)
data Customer = Customer Color BetaDist deriving (Show)

yellowCustomer = Customer Yellow BetaDist { alpha = 2, beta = 5 }
redCustomer = Customer Red BetaDist { alpha = 2, beta = 2 }
blueCustomer = Customer Blue BetaDist { alpha = 5, beta = 1 }

{- 
    
    F(t) = 1 - e ^ (-t / alpha)

    The function above will maps the t to the probability of how customers arrive into the bank.
    
    F :: t -> [0, 1]

    So the inverse function of F(t), namely F'(u) would be

    F'(u) = - alpha * ln (1 - u)

    which we will generate a random u and results in the time [0, Infinity).
-}
gen_probability :: RandomGen g => Double -> g -> Double
gen_probability alpha g = -alpha * log (1 - u)
  where (u, _) = random g

probability_density_fn :: Double -> BetaDist -> Double
probability_density_fn x beta_dist = rho * x ^ (a - 1) * (1 - x) ^ (b - 1)
  where
    rho = 200
    a = alpha beta_dist
    b = beta beta_dist

work :: IO ()
work = threadDelay 1000000 -- 1 second

toBeDone :: Int -> [()]
toBeDone n = replicate n ()

main = do
  let iterateTime = 100
  
  putStrLn $ P.color P.Yellow "Task 1"
  pb <- newProgressBar defStyle 10 (Progress 0 iterateTime ())

  for_ (toBeDone iterateTime) $ \() -> do
    work
    incProgress pb 1
  
  putStrLn "Given only red customers, what are the average and maximum queue lengths in-front of the teller?"
  putStrLn "Which type of customer(yellow, red or blue) gives the cloest value between the averate and maximum customer waiting times?"
