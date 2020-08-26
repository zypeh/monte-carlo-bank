module Main where

import System.Random
import System.ProgressBar
import Control.Concurrent (threadDelay)
import Control.Monad
import qualified System.Console.Pretty as P
import Data.List (genericLength)

-- https://www.wikiwand.com/en/Beta_distribution
data BetaDist = BetaDist { alpha :: Int, beta :: Int } deriving (Show)
data Color = Yellow | Red | Blue deriving (Show)
data Customer = Customer { color :: Color, betaDist :: BetaDist } deriving (Show)

yellowCustomer = Customer {
  color = Yellow,
  betaDist = BetaDist { alpha = 2, beta = 5 }
}

-- redCustomer = Customer Red BetaDist { alpha = 2, beta = 2 }
-- blueCustomer = Customer Blue BetaDist { alpha = 5, beta = 1 }

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

gen_probability' :: Double -> Double -> Double
gen_probability' alpha u = -alpha * log (1 - u)

probability_density_fn :: Double -> BetaDist -> Double
probability_density_fn x beta_dist = rho * x ^ (a - 1) * (1 - x) ^ (b - 1)
  where
    rho = 200
    a = alpha beta_dist
    b = beta beta_dist

average :: (Real a, Fractional b) => [a] -> b
average xs = realToFrac (sum xs) / genericLength xs

main = do
  let iterateTime = 10000000
  putStrLn $ P.color P.Yellow "Task 1"

  pb <- newProgressBar defStyle 10 (Progress 0 iterateTime ())
  res <- replicateM iterateTime $ do
    incProgress pb 1
    let alpha_yellow = (alpha . betaDist) yellowCustomer
    randomVal <- randomIO :: IO Double
    let x = gen_probability' (fromIntegral alpha_yellow) randomVal
    pure $ probability_density_fn x (betaDist yellowCustomer)
  
  putStrLn . P.color P.Yellow $ "Average waiting time: " <> show (average res) <> "second(s)."
  putStrLn . P.color P.Yellow $ "Maximum waiting time: " <> show (maximum res) <> "second(s)."
  putStrLn ""

  putStrLn $ P.color P.Red "Task 2"
  updateProgress pb (const $ Progress 0 iterateTime ())
  replicateM_ iterateTime $ do
    incProgress pb 1
    let alpha_yellow = (alpha . betaDist) yellowCustomer
    randomVal <- randomIO :: IO Double
    let x = gen_probability' (fromIntegral alpha_yellow) randomVal
    pure $ probability_density_fn x (betaDist yellowCustomer)

  -- putStrLn "Which type of customer(yellow, red or blue) gives the cloest value between the averate and maximum customer waiting times?"
