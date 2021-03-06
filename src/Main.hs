module Main where

import System.Random
import System.ProgressBar
import Control.Monad
import qualified System.Console.Pretty as P
import Data.List
import Control.Monad
import Control.Applicative
import Data.Maybe

-- https://www.wikiwand.com/en/Beta_distribution
type RandomVal = Double
type Probability = Double
type TimeInSec = Double

data BetaDist = BetaDist { alpha :: Int, beta :: Int } deriving (Show)
data Color = Yellow | Red | Blue deriving (Show, Enum)
data Customer = Customer { color :: Color, betaDist :: BetaDist } deriving (Show)

yellowCustomer = Customer {
  color = Yellow,
  betaDist = BetaDist { alpha = 2, beta = 5 }
}

redCustomer = Customer {
  color = Red,
  betaDist = BetaDist { alpha = 2, beta = 2 }
}

blueCustomer = Customer {
  color = Blue,
  betaDist = BetaDist { alpha = 5, beta = 1 }
}

{- 
    
    F(t) = 1 - e ^ (-t / alpha)

The function above will maps the t to the probability of how customers arrive into the bank.
    
    F :: t -> [0, 1]

So the inverse function of F(t), namely F'(u) would be

    F'(u) = - alpha * ln (1 - u)

which we will generate a random u and results in the time [0, Infinity).
-}
gen_probability :: Double -> RandomVal -> TimeInSec
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
cumulative_distribution_fn :: Probability -> TimeInSec
cumulative_distribution_fn y = (100 * y ^ 2) - (200 / 3 * y ^ 3)

probability_density_fn :: Probability -> BetaDist -> TimeInSec
probability_density_fn x beta_dist = rho * x ^ (a - 1) * (1 - x) ^ (b - 1)
  where
    rho = 200
    a = alpha beta_dist
    b = beta beta_dist

average :: (Real a, Fractional b) => [a] -> b
average xs = realToFrac (sum xs) / genericLength xs

testCustomer :: BetaDist -> RandomVal -> TimeInSec
testCustomer bd rand = probability_density_fn possibility bd
  where possibility = gen_probability (fromIntegral . alpha $ bd) rand

printStat :: P.Color -> [Double] -> IO ()
printStat c res = do
  putStrLn . P.color c $ "Average waiting time: " <> show (average res) <> " second(s)."
  putStrLn . P.color c $ "Maximum waiting time: " <> show (maximum res) <> " second(s)."
  putStrLn ""

findMinumumGroup :: [Double] -> String
findMinumumGroup numbers = show (toEnum . fromJust $ elemIndex (foldl1' min numbers) numbers :: Color)

main = do
  let iterateTime = 10000000

  putStrLn $ P.color P.Yellow "Task 1"
  pb <- newProgressBar defStyle 10 (Progress 0 iterateTime ())
  printStat P.Yellow <=< replicateM iterateTime $ do
    incProgress pb 1
    randomVal <- randomIO :: IO Double
    pure $ testCustomer (betaDist yellowCustomer) randomVal
  
  putStrLn $ P.color P.Red "Task 2"
  updateProgress pb (const $ Progress 0 iterateTime ())
  printStat P.Red <=< replicateM iterateTime $ do
    incProgress pb 1
    let alpha_red = (alpha . betaDist) redCustomer
    randomVal <- randomRIO (0, 1) :: IO Double
    let y = gen_probability (fromIntegral alpha_red) randomVal
    pure $ cumulative_distribution_fn y

  
  putStrLn "Task 3"
  [pb1, pb2, pb3] <- replicateM 3 $ newProgressBar defStyle 10 (Progress 0 iterateTime ())

  yellowRes <- replicateM iterateTime $ do
    incProgress pb1 1
    randomVal <- randomIO :: IO Double
    pure $ testCustomer (betaDist yellowCustomer) randomVal

  redRes <- replicateM iterateTime $ do
    incProgress pb2 1
    randomVal <- randomIO :: IO Double
    pure $ testCustomer (betaDist redCustomer) randomVal

  blueRes <- replicateM iterateTime $ do
    incProgress pb3 1
    randomVal <- randomIO :: IO Double
    pure $ testCustomer (betaDist blueCustomer) randomVal

  let bucketRes = [yellowRes, redRes, blueRes]
  -- let bucket = zip (enumFrom Yellow) bucketRes -- bounded and no need to organize

  let answer = findMinumumGroup $ fmap (\xs -> abs(average xs - maximum xs)) bucketRes
  putStrLn $ answer <> " customer gives the closest value between the average and maximum customer waiting time."