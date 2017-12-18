
source("functions.R")

## make parameters of these values:

n_bayes_factors <- 1000    # how many draws to estimate expected BF
samples         <- seq(50, 1000, by = 50) # sample 

## ANALYSIS & PLOTTING

thresholds <- c(1/10, 1/3, 3, 10)
quantiles  <- c(0.05, 0.2, 0.5, 0.8, 0.95)

## NULL EFFECT

BF0  <- estimate_expected_bf(0, samples, n_bayes_factors)

## check out the 80% curve; you want an 80% curve that is far down (then
## 80% of the Bayes factors are below, which is good if there is a
## null-effect)

quantiles_0 <- BF_quantiles(BF0, quantiles)
plot_BF_quantiles(quantiles_0, thresholds)

## d = 0.2 - rscale = default

BF02_def <- estimate_expected_bf(0.2, samples, n_bayes_factors)

quantiles_02_def <- BF_quantiles(BF02_def, quantiles[2:4])
plot_BF_quantiles(quantiles_02_def, thresholds)

## d =  0.2 - rscale =  0.2 (narrow) - does  not seem to make  much of a
## difference?

BF02_nar <- estimate_expected_bf(0.2, samples, n_bayes_factors,
                                 rscale = 0.2)

quantiles_02_nar <- BF_quantiles(BF02_nar, quantiles[2:4])
plot_BF_quantiles(quantiles_02_nar, thresholds)

## check out the 20% line: 80% of all Bayes factors are above it; That
## means: you want a 20% curve that is far up. Up to n = 1000 you will
## not reliably obtain evidence in favor of the alternative

#################################################

## d = 0.5

BF05 <- estimate_expected_bf(0.5, samples, n_bayes_factors)

quantiles_05 <- BF_quantiles(BF05, quantiles)
plot_BF_quantiles(quantiles_05, thresholds)

## d = 0.8

BF08 <- estimate_expected_bf(0.8, samples, n_bayes_factors)

quantiles_08 <- BF_quantiles(BF08, quantiles)
plot_BF_quantiles(quantiles_08, thresholds)


save_list <- list(BF0 = BF0, BF02 = BF02_def)

save(save_list, file="BF.data")
