## FLP Project 1 - Functional Knapsack problem solution in Haskell
# Author: Michal Pysik (login: xpysik00)
# Year: 2023

Everything was implemented and simulated annealing had been chosen as the optimized algorithm.

I do not guarantee reliable performance of the optimized algorithm because of the arbirary choice of hyperparameters (alpha, initTemp, maxIters)
in the Main module, but I am fully responsible for the correctness of the algorithm's implementation.
This implementation of simulated annealing returns the first solution it finds, that satisfies the constraints.

As the cooling schedule, exponential multiplicative cooling was used: T_k = T_0 * alpha^k, where T_0 is the initial temperature, alpha is the cooling
rate and k is the current iteration number.
http://what-when-how.com/artificial-intelligence/a-comparison-of-cooling-schedules-for-simulated-annealing-artificial-intelligence/