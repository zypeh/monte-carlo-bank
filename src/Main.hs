{-# LANGUAGE NumericUnderscores #-}
module Main where

import System.Random
-- import System.ProgressBar
import Control.Monad
import qualified System.Console.Pretty as P
import Data.List
import Control.Monad
import Control.Applicative
import Data.Maybe
-- import Debug.Trace

newtype RandomVal = RandomVal Double deriving (Show)
newtype Probability = Probability Double
type TimeInSec = Double

-- https://www.wikiwand.com/en/Beta_distribution
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
timeBetweenCustomerArrives :: RandomVal -> TimeInSec
timeBetweenCustomerArrives (RandomVal u) = -a * log (1 - u)
  where a = 100 -- This is a constant, not from the customer

processTimeFn :: BetaDist -> RandomVal -> TimeInSec
processTimeFn bd (RandomVal u) = rho * u ^ (a - 1) * (1 - u) ^ (b - 1)
  where
    rho = 200
    a = alpha bd
    b = beta bd

average :: (Real a, Fractional b) => [a] -> b
average xs = realToFrac (sum xs) / genericLength xs

printStat :: P.Color -> [TimeInSec] -> IO ()
printStat c res = do
  putStrLn . P.color c $ "Average waiting time: " <> show (average res) <> " second(s)."
  putStrLn . P.color c $ "Maximum waiting time: " <> show (maximum res) <> " second(s)."
  putStrLn ""

findMinumumGroup :: [TimeInSec] -> String
findMinumumGroup numbers = show (toEnum . fromJust $ elemIndex (foldl1' min numbers) numbers :: Color)

data Queue = Queue
  { lastCustomerArrivesIn    :: !TimeInSec
  , lastCustomerCompleteTime :: !TimeInSec
  , customerCompleteTime     :: ![TimeInSec]
  , waitingTime              :: ![TimeInSec]
  , queueLen                 :: ![Int]
  } deriving (Show)

initQueue :: Queue
initQueue = Queue
  { lastCustomerArrivesIn = 0
  , lastCustomerCompleteTime = 0
  , customerCompleteTime = []
  , waitingTime = []
  , queueLen = []
  }

simulate :: BetaDist -> Int -> Queue -> [RandomVal] -> Queue
simulate _ 0 queue _ = queue
simulate bd n queue (randX:randY:rands) = simulate bd (n - 1) queue' rands where

  arrivingTime = timeBetweenCustomerArrives randX
  processTime = processTimeFn bd randY

  start = lastCustomerArrivesIn queue + arrivingTime
  end = start + processTime

  customerCompleteTime' = end : customerCompleteTime queue
  waitingTime' = max (lastCustomerCompleteTime queue - start) 0
  newQueueLen = length $ filter (> start) customerCompleteTime' -- this is the bottleneck, consider throwing more memory to GHC

  queue' = Queue
    { lastCustomerArrivesIn = start
    , lastCustomerCompleteTime = end
    , customerCompleteTime = customerCompleteTime'
    , waitingTime = waitingTime' : waitingTime queue
    , queueLen = newQueueLen : queueLen queue -- laziness here to make counting average & maximum fast
    }

runSimulate :: RandomGen g => Customer -> Int -> g -> Queue
runSimulate c n g = simulate (betaDist c) n initQueue (RandomVal <$> (take (n*2) (randoms g)))

main = do
  let iterateTime = 100_000

  seed <- newStdGen
  putStrLn "Task 1"
  let yellowQueue = runSimulate yellowCustomer iterateTime seed
  printStat P.Yellow (waitingTime yellowQueue)
  
  putStrLn "Task 2"
  let redQueue = runSimulate redCustomer iterateTime seed
  printStat P.Red (waitingTime redQueue)

  putStrLn . P.color P.Red $ "For red customers, the average queue length is " <> show (average . queueLen $ redQueue)
  putStrLn . P.color P.Red $ "For red customers, the maximum queue length is " <> show (maximum . queueLen $ redQueue)
  putStrLn ""

  putStrLn "Task 3"
  let blueQueue = runSimulate blueCustomer iterateTime seed
  printStat P.Blue (waitingTime blueQueue)

  let bucketRes = waitingTime <$> [yellowQueue, redQueue, blueQueue]
  let answer = findMinumumGroup $ fmap (\xs -> abs(average xs - maximum xs)) bucketRes
  putStrLn $ answer <> " customer gives the closest value between the average and maximum customer waiting time."