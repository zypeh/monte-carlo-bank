# monte-carlo-bank

A Haskell code to run Monte Carlo method regarding the probability of time spent in front of the counter staff.

### `v2` branch
The v2 branch is to fix the simulation problem. The logic in the master branch is wrong and produced the wrong results.

`v2 branch` is aimed to fix it via proper mindset, which is more likely to "simulation" instead of calculation.

1. Set the time duration (eg. 60 seconds)
2. Count the number of customer via probility in the time duration
3. For each customer, count the time taken to execute task via the probability beta distribution.

### How to run
Via `stack`, just run command `stack run`.
