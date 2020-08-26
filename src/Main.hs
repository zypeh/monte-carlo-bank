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

redCustomer = Customer {
  color = Red,
  betaDist = BetaDist { alpha = 2, beta = 2 }
}

{- 
    
    F(t) = 1 - e ^ (-t / alpha)

The function above will maps the t to the probability of how customers arrive into the bank.
    
    F :: t -> [0, 1]

So the inverse function of F(t), namely F'(u) would be

    F'(u) = - alpha * ln (1 - u)

which we will generate a random u and results in the time [0, Infinity).
-}
gen_probability :: Double -> Double -> Double
gen_probability alpha u = -alpha * log (1 - u)

{-
To calculate the queue lengths in-front of the teller. We need to accumulate the possible time of
the customers til time @t@. Given the function

    F(x) = 200 * x (alpha - 1) * (1 - x) ^ (beta - 1)
      where
        alpha = 2, beta = 2 for red customer.

    F(x) = 200x(1-x)

Integrate the density of these graph to get the accumulative function, @Cum@. So

    Cum(y) = \int_{0}^{y} 200x (1-x) dx
           = 200 \int_{0}&{y} x - x^2 dx
           = 200[y^2 / 2 - y^3 / 3]
           = 100 * y^2 - 200 / 3 * y ^3
-}
cumulative_distribution_fn :: Double -> Double
cumulative_distribution_fn y = (100 * y ^ 2) - (200 / 3 * y ^ 3)

probability_density_fn :: Double -> BetaDist -> Double
probability_density_fn x beta_dist = rho * x ^ (a - 1) * (1 - x) ^ (b - 1)
  where
    rho = 200
    a = alpha beta_dist
    b = beta beta_dist

average :: (Real a, Fractional b) => [a] -> b
average xs = realToFrac (sum xs) / genericLength xs

main = do
  let iterateTime = 1000000
  
  putStrLn $ P.color P.Yellow "Task 1"
  pb <- newProgressBar defStyle 10 (Progress 0 iterateTime ())
  res <- replicateM iterateTime $ do
    incProgress pb 1
    let alpha_yellow = (alpha . betaDist) yellowCustomer
    randomVal <- randomRIO (0, 1) :: IO Double
    let x = gen_probability (fromIntegral alpha_yellow) randomVal
    pure $ probability_density_fn x (betaDist yellowCustomer)
  
  putStrLn . P.color P.Yellow $ "Average waiting time: " <> show (average res) <> " second(s)."
  putStrLn . P.color P.Yellow $ "Maximum waiting time: " <> show (maximum res) <> " second(s)."
  putStrLn ""

  putStrLn $ P.color P.Red "Task 2"
  updateProgress pb (const $ Progress 0 iterateTime ())
  res2 <- replicateM iterateTime $ do
    incProgress pb 1
    let alpha_red = (alpha . betaDist) redCustomer
    randomVal <- randomRIO (0, 1) :: IO Double
    let y = gen_probability (fromIntegral alpha_red) randomVal
    pure $ cumulative_distribution_fn y

  putStrLn . P.color P.Red $ "Average waiting time: " <> show (average res2) <> " second(s)."
  putStrLn . P.color P.Red $ "Maximum waiting time: " <> show (maximum res2) <> " second(s)."
  putStrLn ""

  -- putStrLn "Which type of customer(yellow, red or blue) gives the cloest value between the averate and maximum customer waiting times?"
