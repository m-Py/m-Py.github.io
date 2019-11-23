
# Load packages that are needed for the simulation
library(MASS)
library(varBF)

# Load some packages for data handling that is needed lateron
library(dplyr)
library(tidyr)
library(ggplot2)

# Function to simulate paired data
paired_data <- function(n, m1, m2, sd1, sd2, r) {
  cor_matrix <- matrix(c(1, r, r, 1), ncol = 2)
  sds <- c(sd1, sd2)
  vars <- sds %*% t(sds)
  cov_matrix <- vars * cor_matrix
  MASS::mvrnorm(n, mu = c(m1, m2), Sigma = cov_matrix)
}

# Function to simulate paired data and then compute a Bayes factor
compare_correlated_variances <- function(X, n, m1, m2, sd1, sd2, r) {
  pairs <- paired_data(n, m1, m2, sd1, sd2, r)
  varBF::depvarBF(pairs[, 1], pairs[, 2])
}

# A simulation function, running the above functions often (specified via the argument `nruns`)
simulate_bfs <- function(nruns, n, m1, m2, sd1, sd2, r) {
  # Generate 100 bivariate data sets and compute a Bayes factor each time:
  BFs <- lapply(
    X = 1:nruns, 
    FUN = compare_correlated_variances,
    n = n,
    m1 = m1,
    m2 = m2,
    sd1 = sd1,
    sd2 = sd2,
    r = r
  )
  BFs <- simplify2array(BFs, higher = FALSE)
  BFs <- data.frame(BayesFactor = BFs, N = n)
  BFs$nruns <- nruns
  BFs$m1 <- m1
  BFs$m2 <- m2
  BFs$sd1 <- sd1
  BFs$sd2 <- sd2
  BFs$r <- r
  BFs
}


# Vary input parameters
conditions <- expand.grid(
  r = c(0, 0.1, 0.3, 0.5, 0.7), 
  sd2 = c(11, 13, 15),
  n = seq(20, 200, by = 20)
)

# Do the simulation, 1,000 runs per parameter combination
start <- Sys.time()
runs <- list()
nruns <- 1000
for (i in 1:nrow(conditions)) {
  runs[[i]] <- simulate_bfs(
    nruns = nruns,
    n = conditions$n[i],
    m1 = 100,
    m2 = 100,
    sd1 = 15,
    sd2 = conditions$sd2[i],
    r = conditions$r[i]
  )
}
# Combine all data frame into a single one:
all_runs <- do.call(rbind, runs)
Sys.time() - start


# Check out the resulting data frame:
nrow(all_runs)
head(all_runs)

#########

## Analyze the data simulated above

# Compute some quantiles for BFs
quants <- all_runs %>% 
  group_by(N, sd2) %>% 
  summarise(
    pt20 = quantile(BayesFactor, 0.2), 
    pt50 = quantile(BayesFactor, 0.5), 
    pt80 = quantile(BayesFactor, 0.8)
  ) %>%
  ungroup() %>%
  pivot_longer(
    cols = starts_with("pt"), 
    names_to = "quant", 
    values_to = "BayesFactor",
    names_prefix = "pt"
  )

# Plot the quantiles on a log scale - facetted by N 
scaleFUN <- function(x) sprintf("%.2f", x)
png(filename = "Rplot001.png", width = 1200, height = 640, pointsize = 48)
quants %>%
  mutate(Effect = factor(paste("SD1 = 15, SD2 =", sd2))) %>%
  ggplot(aes(x = N, y = BayesFactor, colour = quant)) + 
  geom_line() + 
  geom_point(size = 5) + 
  facet_grid(~ Effect) + 
  geom_hline(yintercept = 1, size = 1.3) +
  scale_y_continuous(trans = "log", breaks = c(0, 0.1, 1/3, 1, 5, 10, 20, 50, 100, 1000, 10000, 100000), labels = scaleFUN) + 
  theme_bw(base_size = 22)
dev.off()


# Plot the quantiles on a log scale - facetted by r

quants_r <- all_runs %>% 
  group_by(r, sd2) %>% 
  summarise(
    pt20 = quantile(BayesFactor, 0.2), 
    pt50 = quantile(BayesFactor, 0.5), 
    pt80 = quantile(BayesFactor, 0.8)
  ) %>%
  ungroup() %>%
  pivot_longer(
    cols = starts_with("pt"), 
    names_to = "quant", 
    values_to = "BayesFactor",
    names_prefix = "pt"
  )

png(filename = "Rplot002.png", width = 1200, height = 640, pointsize = 48)
quants_r %>%
  mutate(Effect = factor(paste("SD1 = 15, SD2 =", sd2))) %>%
  ggplot(aes(x = r, y = BayesFactor, colour = quant)) + 
  geom_line() + 
  geom_point(size = 5) + 
  facet_grid(~ Effect) + 
  geom_hline(yintercept = 1, size = 1.3) +
  scale_y_continuous(trans = "log", breaks = c(0, 0.1, 1/3, 1, 5, 10, 20, 50, 100, 1000, 10000, 100000), labels = scaleFUN) + 
  theme_bw(base_size = 22)
dev.off()
