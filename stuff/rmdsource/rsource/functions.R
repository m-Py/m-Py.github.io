
library("BayesFactor")
library("reshape2")
library("plyr")

## Compute "power" of the design: given N, effect size, prior scale, and
## a fixed probability X: what is the Bayes factor value that X% will
## lie above (later: below if the effect is null); nsim guides the
## precision of the estimate (more nsim = more simulations to estimate
## the value)

power_bf <- function(N, effect_size, nsim = 1000, rscale = sqrt(2)/2,
                     probability = 0.8, say_result = TRUE) {
    info <- BF_quantiles(estimate_expected_bf(effect_size, N, nsim, rscale),
                         1-probability)
    info$BF <- exp(info$logBF) # other functions return the log BF
    info$quantile <- NULL
    info$power    <- probability
    if (say_result) {
        cat("\n Approximately", probability*100,
            "% of all Bayes factors will be larger than",
            round(info$BF, 2), "\n\n")
    }
    return(info)
}

## Returns a data.frame in with columns N and BF; each BF is based on
## one draw in the simulation. `sample_sizes` is the »total sample
## size«, i.e. the sample size per group is `sample_sizes/2`. Note that
## the log(BF) is returned!
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
        ## store all bayes factors for sample size i (store the log BF!)
        samples_bf[[i]] <- log(repetitions_bf)
    }
    ## convert to data.frame in long format:
    ret <- data.frame(logBF = unlist(samples_bf),
                      N  = rep(sample_sizes, each = n_bayes_factors),
                      eff_size = effect_size,
                      rscale = rscale)
    return(ret)
}

## processes data returned by `estimate_expected_bf` and returns Bayes
## factor quantiles by sample size as a data.frame. Note that the
## log(BF) is returned!
BF_quantiles <- function(dat, quantiles = c(0.2, 0.5, 0.8)) {
    foo <- tapply(dat$logBF, dat$N, quantile, probs=quantiles,
                  simplify = FALSE)
    ## use plyr to convert array to data.frame
    foo <- adply(foo, 1)
    ## use reshape to create long data
    long_quantiles <- melt(foo, id.vars= c("X1"), factorsAsStrings=TRUE)
    ## some ugly renaming:
    long_quantiles$N <- as.numeric(levels(long_quantiles$X1))[long_quantiles$X1]
    long_quantiles$X1 <- NULL
    long_quantiles$quantile <- long_quantiles$variable
    long_quantiles$variable <- NULL
    long_quantiles$logBF    <- long_quantiles$value
    long_quantiles$value    <- NULL
    ## attach effect size and prior scale
    long_quantiles$eff_size <- unique(dat$eff_size)
    long_quantiles$rscale   <- unique(dat$rscale)
    return(long_quantiles)
}

plot_BF_quantiles <- function(BF_quantiles, thresholds=NULL, ylim=NULL,
                              main = "", axis = c("log", "standard")
                              ) {

    ## first make some adjustments to the plot in dependence of which
    ## axis the user wants to show:
    ylab <- ""
    ## adjust margin to make space for a second y-axis -- ‘c(bottom,
    ## left, top, right)’; The default ‘c(5, 4, 4, 2) + 0.1’.
    def_mar <- c(5, 4, 4, 2) + 0.1
    cur_mar <- def_mar
    if ("standard" %in% axis) {
        cur_mar <- cur_mar + c(0,0,0,2)
        par(mar = cur_mar)
    }
    else {
        cur_mar <- cur_mar + c(0,0,0,-2)
        par(mar = cur_mar)
    }
    if ("log" %in% axis) {
        draw_y <- "s"
        ylab <- "log(BF)"
    } else {
        draw_y <- "n"
        cur_mar <- cur_mar + c(0,-2,0,0)
        par(mar = cur_mar)
    }

    ## draw the actual plot
    plot(BF_quantiles$N, BF_quantiles$logBF, pch = 3,
         col = rep(1:length(unique(BF_quantiles$quantile)),
         each = length(unique(BF_quantiles$N))), ylab = ylab,
         xlab ="Sample size", las=1, ylim = ylim, main=main,
         yaxt = draw_y)

    ## add y-axis to the right if that was required by the user
    if ("standard" %in% axis)  add_normal_bf_scale()

    ## indicate the neutral evidence line if it was required by the user
    cols = rep("darkgrey", length(thresholds))
    cols[which(thresholds==1)] <- "black"
    abline(h = log(thresholds), lty=2, lwd =1.5, col=cols)
    
    ## reset margins
    on.exit(par(mar = def_mar))
}

add_normal_bf_scale <- function() {
    label_bf_lim <- seq(-30, 30, by = 0.5)
    labs <- round(exp(label_bf_lim), 2)
    labs[labs >=10] <- round(labs[labs >=10])
    labs[labs >=1] <- round(labs[labs >=1], 1)
    axis(4, at = label_bf_lim, labels = labs, las=1)
    mtext("BF", 4, line = 3.1)
}
