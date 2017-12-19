
source("functions.R")

n_bayes_factors <- 1000    # how many draws to estimate expected BF
samples         <- seq(50, 1000, by = 50) # sample 

## Simulate the distribution of Bayes factors by sample size and effect
## size

## NULL EFFECT

BF0  <- estimate_expected_bf(0, samples, n_bayes_factors)

## check out the 80% curve; you want an 80% curve that is far down (then
## 80% of the Bayes factors are below, which is good if there is a
## null-effect)

## d = 0.2

BF02 <- estimate_expected_bf(0.2, samples, n_bayes_factors)

## check out the 20% line: 80% of all Bayes factors are above it; That
## means: you want a 20% curve that is far up. Up to n = 1000 you will
## not reliably obtain evidence in favor of the alternative

#################################################

## d = 0.5

## need different sample sizes because the Bayes factor becomes very
## large very fast for medium and large effects
samples <- seq(20, 200, by = 10) 

BF05 <- estimate_expected_bf(0.5, samples, n_bayes_factors)

## d = 0.8

BF08 <- estimate_expected_bf(0.8, samples, n_bayes_factors)

defBF <- list(BF0 = BF0, BF02 = BF02, BF05 = BF05, BF08 = BF08)

save(defBF, file="../data/defBF.data")

