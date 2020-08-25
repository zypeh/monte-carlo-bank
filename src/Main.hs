module Main where

-- https://www.wikiwand.com/en/Beta_distribution
data BetaDist = BetaDist { alpha :: Int, beta :: Int } deriving (Show)
data Color = Yellow | Red | Blue deriving (Show)
data Customer = Customer Color BetaDist deriving (Show)

yellowCustomer = Customer Yellow BetaDist { alpha = 2, beta = 5 }
redCustomer = Customer Red BetaDist { alpha = 2, beta = 2 }
blueCustomer = Customer Blue BetaDist { alpha = 5, beta = 1 }

-- the average waiting time
meanOf :: BetaDist -> Int
meanOf beta_dist = a `div` (a + b)
  where
    a = alpha beta_dist
    b = beta beta_dist

-- maximum waiting time
modeOf :: BetaDist -> Int
modeOf beta_dist = (a - 1) `div` (a + b - 2)
  where
    a = alpha beta_dist
    b = beta beta_dist

-- accumulative_distribution_fn :: Int -> BetaDist -> Int
-- accumulative_distribution_fn x beta_dist = 1 - e ^ (negate x)

probability_density_fn :: Int -> BetaDist -> Int
probability_density_fn x beta_dist = rho * x ^ (a - 1) * (1 - x) ^ (b - 1)
  where
    rho = 200
    a = alpha beta_dist
    b = beta beta_dist

main :: IO ()
main = do
  putStrLn "hello world"
