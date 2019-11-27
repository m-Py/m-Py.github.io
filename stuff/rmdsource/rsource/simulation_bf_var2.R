
library(dplyr)
library(ggplot2)
library(varBF)
library(car)
library(BFpack)
library(tidyr)

# Function to compare variances from two independent groups
#
# param x: The first numeric vector, data points from one sample
# param y: The second numeric vector, data points from the other sample
# return: p values and Bayes factors for Bartlett and Levene test

compare_variances <- function(x, y) {
  nx <- length(x) 
  ny <- length(y)
  
  # Levene test
  lt <- car::leveneTest(
    y = c(x, y),
    group = factor(c(rep(1, nx), rep(2, ny)))
  )
  
  # Bartlett test 
  bt <- BFpack::bartlett_test(
    x = c(x, y),
    g = c(rep(1, nx), rep(2, ny))
  )
  
  # Bartlett-Mulder Bayes factor
  posteriors <- BFpack::BF(bt)$PHP_exploratory
  BFB <- posteriors[2] / posteriors[1]

  # Levene-Anderson Bayes factor
  BFL <- varBF::indepvarBF(x, y)
  
  # Output
  output <- c(
    P_Levene = lt[["Pr(>F)"]][1],
    P_Bartlett = bt$p.value,
    BF_Levene = BFL,
    BF_Bartlett = unname(BFB)
  )
  output
}

# This function allows a multiple length vector `N` and `sd2` as input 
# for allowing to vary `N` and the effect size between simulation runs.
# The parameter `nruns` then refers to the simulation runs for each 
# combination of `N` and `sd2`!
simulate_variance_comparison <- function(nruns, N, sd2) {
  conditions <- expand.grid(N = N , sd2 = sd2)
  results <- list()
  for (i in 1:nrow(conditions)) {
    sim_runs <- lapply(
      X = 1:nruns,
      simulate_variance_comparison_,
      N = conditions$N[i],
      sd2 = conditions$sd2[i]
    )
    results[[i]] <- data.frame(t(simplify2array(sim_runs, higher = FALSE)))
    results[[i]]$N <- conditions$N[i]
    results[[i]]$sd2 <- conditions$sd2[i]
  }
  
  do.call(rbind, results)
}

# helper function; generates data and computes BF for a single simulation run
simulate_variance_comparison_ <- function(X, N, sd2) {
  group1 <- rnorm(N, mean = 100, sd = 15) # for log normal data: wrap rnorm by log()
  group2 <- rnorm(N, mean = 100, sd = sd2)
  compare_variances(group1, group2)
}

#### Simulation 3: There is an effect

comparisons <- simulate_variance_comparison(
  nruns = 1000, 
  N = seq(from = 50, to = 300, by = 50), 
  sd2 = c(18, 20, 22)
)

head(comparisons)

# encode the simulation run:
comparisons$run <- 1:nrow(comparisons)

# convert into long format, i.e., Bayes factor is a condition
ldf <- comparisons %>%
  select(-(1:2)) %>% # drop p values
  pivot_longer(
    cols = starts_with("BF"), 
    names_to = "Method", 
    values_to = "BayesFactor",
    names_prefix = "BF_"
  )

head(ldf)


# Compute quantiles by method and N, and plot the information
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

png(filename = "sim3-post15.png", width = 800, height = 640, pointsize = 48)
scaleFUN <- function(x) sprintf("%.2f", x)
quantiles_effect %>%
  ggplot(aes(x = N, y = BayesFactor, colour = Quantile)) + 
  geom_line(aes(lty = Method)) + 
  geom_point(aes(shape = Method), size = 5) + 
  scale_color_manual(values = c('#383745', '#A17724', '#9E9CC2')) + 
  scale_y_continuous(trans = "log", breaks = c(0, 0.1, 1/3, 1, 5, 10, 20, 50, 100, 1000, 10000, 100000), labels = scaleFUN) + 
  ylab("BF 10") + 
  ggtitle("Normal data, variances are not homogenous") +
  theme_bw(base_size = 22)
dev.off()


### Simulation 4: There is no effect

comparisons <- simulate_variance_comparison(
  nruns = 1000, 
  N = seq(from = 50, to = 300, by = 50), 
  sd2 = c(15) # null effect
)
# encode the simulation run:
comparisons$run <- 1:nrow(comparisons)

# convert into long format, i.e., Bayes factor is a condition
ldf <- comparisons %>%
  select(-(1:2)) %>% # drop p values
  pivot_longer(
    cols = starts_with("BF"), 
    names_to = "Method", 
    values_to = "BayesFactor",
    names_prefix = "BF_"
  ) 

head(ldf)


# Compute BF quantiles by method and N, and plot the information
quantiles_null <- ldf %>% 
  mutate(BayesFactor = 1 / BayesFactor) %>% # use BF01
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

png(filename = "sim4-post15.png", width = 800, height = 640, pointsize = 48)
quantiles_null %>%
  ggplot(aes(x = N, y = BayesFactor, colour = Quantile)) + 
    geom_line(aes(lty = Method)) + 
    geom_point(aes(shape = Method), size = 5) + 
    scale_color_manual(values = c('#383745', '#A17724', '#9E9CC2')) + 
    scale_y_continuous(trans = "log", breaks = c(0, 0.1, 1/3, 1, 3, 5, 10, 20), labels = scaleFUN) + 
    ylab("BF 01") + 
    theme_bw(base_size = 22) + 
    ggtitle("Normal data, variances are homogenous")
dev.off()

### Simulation 5: log normal data -- variances differ


# helper function; generates data and computes BF for a single simulation run
simulate_variance_comparison_ <- function(X, N, sd2) {
  group1 <- log(rnorm(N, mean = 100, sd = 15)) # for log normal data: wrap rnorm by log()
  group2 <- log(rnorm(N, mean = 100, sd = sd2))
  compare_variances(group1, group2)
}

comparisons <- simulate_variance_comparison(
  nruns = 1000, 
  N = seq(from = 50, to = 300, by = 50), 
  sd2 = c(18, 20, 22)
)

head(comparisons)

# encode the simulation run:
comparisons$run <- 1:nrow(comparisons)

# convert into long format, i.e., Bayes factor is a condition
ldf <- comparisons %>%
  select(-(1:2)) %>% # drop p values
  pivot_longer(
    cols = starts_with("BF"), 
    names_to = "Method", 
    values_to = "BayesFactor",
    names_prefix = "BF_"
  )

head(ldf)


# Compute quantiles by method and N, and plot the information
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

png(filename = "sim3-post15-log.png", width = 800, height = 640, pointsize = 48)
scaleFUN <- function(x) sprintf("%.2f", x)
quantiles_effect %>%
  ggplot(aes(x = N, y = BayesFactor, colour = Quantile)) + 
  geom_line(aes(lty = Method)) + 
  geom_point(aes(shape = Method), size = 5) + 
  scale_color_manual(values = c('#383745', '#A17724', '#9E9CC2')) + 
  scale_y_continuous(trans = "log", breaks = c(0, 0.1, 1/3, 1, 5, 10, 20, 50, 100, 1000, 10000, 100000), labels = scaleFUN) + 
  ylab("BF 10") + 
  ggtitle("Log-Normal data, variances are not homogenous") +
  theme_bw(base_size = 22)
dev.off()

### Simulation 6: log normal data -- variances do not differ
comparisons <- simulate_variance_comparison(
  nruns = 1000, 
  N = seq(from = 50, to = 300, by = 50), 
  sd2 = c(15) # null effect
)
# encode the simulation run:
comparisons$run <- 1:nrow(comparisons)

# convert into long format, i.e., Bayes factor is a condition
ldf <- comparisons %>%
  select(-(1:2)) %>% # drop p values
  pivot_longer(
    cols = starts_with("BF"), 
    names_to = "Method", 
    values_to = "BayesFactor",
    names_prefix = "BF_"
  ) 

head(ldf)


# Compute BF quantiles by method and N, and plot the information
quantiles_null <- ldf %>% 
  mutate(BayesFactor = 1 / BayesFactor) %>% # use BF01
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

png(filename = "sim4-post15-log.png", width = 800, height = 640, pointsize = 48)
quantiles_null %>%
  ggplot(aes(x = N, y = BayesFactor, colour = Quantile)) + 
    geom_line(aes(lty = Method)) + 
    geom_point(aes(shape = Method), size = 5) + 
    scale_color_manual(values = c('#383745', '#A17724', '#9E9CC2')) + 
    scale_y_continuous(trans = "log", breaks = c(0, 0.1, 1/3, 1, 3, 5, 10, 20), labels = scaleFUN) + 
    ylab("BF 01") + 
    theme_bw(base_size = 22) + 
    ggtitle("Log-Normal data, variances are homogenous")
dev.off()
