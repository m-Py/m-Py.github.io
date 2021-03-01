
library(dplyr)
library(ggplot2)
library(tidyr)
library(BayesFactor)
library(abtest)

#' Function to compare variances from two independent groups
#'
#' @param x: The first numeric vector, vector of successes in condition 1
#' @param y: The second numeric vector, vector of successes in condition 1
#' @return Two Bayes factors (abtest, BayesFactor)
#' @examples
#' x <- rbinom(100, 1, .5)
#' y <- rbinom(100, 1, .7)
#' compare_proportions(x, y)

compare_proportions <- function(x, y) {

  successes <- matrix(
    c(sum(x == 1), sum(y == 1),
      sum(x == 0), sum(y == 0)),
    byrow = TRUE,
    ncol = 2
  )

  # Do Kass / Vaidyanathan Test (`abtest` package)
  ab_test_data <- list(
    y1 = successes[1, 1],
    n1 = sum(successes[, 1]),
    y2 = successes[1, 2],
    n2 = sum(successes[, 1])
  )

  bf_abtest <- abtest::ab_test(data = ab_test_data)
  bf_abtest <- bf_abtest$bf$bf10
  
  # Do Gunel / Dickey Test (`BayesFactor` package)
  bf_bayesfactor <- contingencyTableBF(
    successes, 
    sampleType = "indepMulti", 
    fixedMargin = "cols"
  )
  bf_bayesfactor <- extractBF(bf_bayesfactor)$bf
  c(GD = bf_bayesfactor, KV = bf_abtest)
}

# This function allows a multiple length vectors `N`, `p1`, and `p2` as input 
# for allowing to vary `N` and the proportions between simulation runs.
# The parameter `nruns` then refers to the simulation runs for each 
# parameter combination! N is the sample size FOR EACH CONDITION.
simulate_proportion_comparison <- function(nruns, N, p1 = .5, p2) {
  conditions <- expand.grid(N = N, p1 = p1, p2 = p2)
  results <- list()
  for (i in 1:nrow(conditions)) {
    sim_runs <- lapply(
      X = 1:nruns,
      simulate_proportion_comparison_,
      N = conditions$N[i],
      p1 = conditions$p1[i],
      p2 = conditions$p2[i]
    )
    results[[i]] <- data.frame(t(simplify2array(sim_runs, higher = FALSE)))
    results[[i]]$N <- conditions$N[i]
    results[[i]]$p1 <- conditions$p1[i]
    results[[i]]$p2 <- conditions$p2[i]
  }
  do.call(rbind, results)
}

# helper function; generates data and computes BF for a single simulation run
simulate_proportion_comparison_ <- function(X, N, p1, p2) {
  x <- rbinom(N, 1, p1)
  y <- rbinom(N, 1, p2)
  compare_proportions(x, y)
}

#### Simulation 1: There is no effect

nruns <- 500

comparisons0 <- simulate_proportion_comparison(
  nruns = nruns, 
  N = c(seq(from = 50, to = 300, by = 50), 1000), 
  p1 = 0.5,
  p2 = 0.5
)

head(comparisons0)

# encode the simulation run:
comparisons0$run <- 1:nrow(comparisons0)

# convert into long format, i.e., Bayes factor is a condition
ldf <- comparisons0 %>%
  pivot_longer(
    cols = c("GD", "KV"),
    names_to = "Method", 
    values_to = "BayesFactor",
    names_prefix = "BF_"
  )

head(ldf)


# Compute quantiles by method and N, and plot the information
quantiles_null <- ldf %>% 
  group_by(N, Method) %>% 
  summarise(
    pt20 = quantile(BayesFactor, 0.2), 
    pt50 = quantile(BayesFactor, 0.5), 
    pt80 = quantile(BayesFactor, 0.8)
  ) %>%
  ungroup() %>%
  pivot_longer(
    cols = starts_with("pt"), 
    names_to = "Quantile", 
    values_to = "BayesFactor",
    names_prefix = "pt"
  ) %>%
  mutate(Quantile = ordered(
    paste0(Quantile, "%"), 
    levels = paste0(c(80, 50, 20), "%"))
  )

png(filename = "Sim1-PropBF.png", width = 800, height = 640, pointsize = 48)
scaleFUN <- function(x) sprintf("%.2f", x)
quantiles_null %>%
  mutate(BayesFactor = 1 / BayesFactor) %>% # use BF01
  ggplot(aes(x = N, y = BayesFactor, colour = Quantile)) + 
  geom_line(aes(lty = Method)) + 
  geom_point(aes(shape = Method), size = 5) + 
  scale_color_manual(values = c('#383745', '#A17724', '#9E9CC2')) + 
  scale_y_continuous(trans = "log", breaks = c(0, 0.1, 1/3, 1, 5, 10, 20, 50, 100, 1000, 10000, 100000), labels = scaleFUN) + 
  ylab("BF 01") + 
  theme_bw(base_size = 22) +
    ggtitle("There is no effect.")
dev.off()


### Simulation 2: There is an effect

comparisons1 <- simulate_proportion_comparison(
  nruns = nruns, 
  N = c(seq(from = 50, to = 300, by = 50), 1000), 
  p1 = 0.7,
  p2 = 0.5
)

# encode the simulation run:
comparisons1$run <- 1:nrow(comparisons1)

# convert into long format, i.e., Bayes factor is a condition
ldf <- comparisons1 %>%
  pivot_longer(
    cols = c("GD", "KV"),
    names_to = "Method", 
    values_to = "BayesFactor",
    names_prefix = "BF_"
  )

# Compute BF quantiles by method and N, and plot the information
quantiles_effect <- ldf %>% 
  group_by(N, Method) %>% 
  summarise(
    pt20 = quantile(BayesFactor, 0.2), 
    pt50 = quantile(BayesFactor, 0.5), 
    pt80 = quantile(BayesFactor, 0.8)
  ) %>%
  ungroup() %>%
  pivot_longer(
    cols = starts_with("pt"), 
    names_to = "Quantile", 
    values_to = "BayesFactor",
    names_prefix = "pt"
  ) %>%
  mutate(Quantile = ordered(
    paste0(Quantile, "%"), 
    levels = paste0(c(80, 50, 20), "%"))
  )

png(filename = "Sim2-PropBF.png", width = 800, height = 640, pointsize = 48)
quantiles_effect %>%
  ggplot(aes(x = N, y = BayesFactor, colour = Quantile)) + 
    geom_line(aes(lty = Method)) + 
    geom_point(aes(shape = Method), size = 5) + 
    scale_color_manual(values = c('#383745', '#A17724', '#9E9CC2')) + 
    scale_y_continuous(trans = "log", breaks = c(0, 0.1, 1/3, 1, 3, 5, 10, 20), labels = scaleFUN) + 
    ylab("BF 10") + 
    theme_bw(base_size = 22) +
    ggtitle("There is an effect.")
dev.off()
