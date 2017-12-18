
library("BayesFactor")

## Returns a data.frame in with columns N and BF; each BF is based on
## one draw in the simulation. `sample_sizes` is the »total sample
## size«, i.e. the sample size per group is `sample_sizes/2`
estimate_expected_bf <- function(effect_size, sample_sizes,
                                 n_bayes_factors, rscale=sqrt(2)/2) {
    ## store BFs for each repetition for one sample size:
    repetitions_bf <- vector(length=n_bayes_factors)
    ## store BFs by sample size:
    samples_bf     <- list()
    
    for (i in 1:length(sample_sizes)) {
        for (j in 1:n_bayes_factors) {
            group0 <- rnorm(sample_sizes[i]/2, 0, 1)
            group1 <- rnorm(sample_sizes[i]/2, effect_size, 1)
            repetitions_bf[j] <- extractBF(ttestBF(group0, group1, rscale=rscale))$bf
        }
        ## store all bayes factors for sample size i
        samples_bf[[i]] <- log(repetitions_bf)
    }
    ## convert to data.frame in long format:
    ret <- data.frame(BF = unlist(samples_bf),
                      N  = rep(sample_sizes, each = n_bayes_factors),
                      eff_size = effect_size,
                      rscale = rscale)
    return(ret)
}

## processes data returned by `estimate_expected_bf` and returns Bayes
## factor quantiles by sample size as a data.frame
BF_quantiles <- function(dat, quantiles = c(0.025, 0.5, 0.975)) {
    foo <- tapply(dat$BF, dat$N, quantile, probs=quantiles)
    ## use plyr to convert array to data.frame
    foo <- plyr::adply(foo, 1)
    ## use reshape to create long data
    long_quantiles <- reshape2::melt(foo, id.vars= c("X1"), factorsAsStrings=TRUE)
    long_quantiles$N <- as.numeric(levels(long_quantiles$X1))[long_quantiles$X1]
    long_quantiles$X1 <- NULL
    long_quantiles$eff_size <- unique(dat$eff_size)
    long_quantiles$rscale   <- unique(dat$rscale)
    return(long_quantiles)
}

plot_BF_quantiles <- function(BF_quantiles, thresholds=NULL) {
    ## adjust margin to make space for a second y-axis -- ‘c(bottom,
    ## left, top, right)’; The default ‘c(5, 4, 4, 2) + 0.1’.
    def_mar <- c(5, 4, 4, 2) + 0.1 
    par(mar = def_mar + c(0,0.3,-2,2))
    plot(BF_quantiles$N, BF_quantiles$value, pch = 3,
         col = rep(1:length(unique(BF_quantiles$variable)),
         each = length(unique(BF_quantiles$N))), ylab = "log(BF10)",
         xlab ="Sample size", las=1)
    label_bf_lim <- seq(-30, 30, by = 0.5)
    axis(4, at = label_bf_lim, labels = round(exp(label_bf_lim), 2), las=1)
    mtext("BF10", 4, line = 3.1)
    abline(h = log(thresholds), lty=2, lwd =1.5, col = "darkgrey")
    par(mar = def_mar)
}
