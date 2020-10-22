
library(anticlust)

source("27_Slow_Kmeans.R")

N <- 100
M <- 2
K <- 2
data <- matrix(rnorm(N * M), ncol = M)
clusters <- sample(rep_len(1:K, N))

methods <- c(
  "non-vectorized", 
  "vectorized", 
  "local-update",
  "C"
)

times <- rep(NA, length(methods))
names(times) <- methods
cluster_solutions <- matrix(NA, ncol = length(methods), nrow = N)
colnames(cluster_solutions) <- methods

get_seconds <- function(t1, t2) {
  as.numeric(difftime(t2, t1, units = "secs"))
}

## 1. Using non-vectorized code
start <- Sys.time()
cluster_solutions[, "non-vectorized"]  <- anticlustering(
  data,
  K = clusters,
  objective = variance_objective_slow
)
times["non-vectorized"] <- get_seconds(start, Sys.time())

## 2. Using vectorized code

start <- Sys.time()
cluster_solutions[, "vectorized"] <- anticlustering(
  data,
  K = clusters,
  objective = variance_objective
)
times["vectorized"] <- get_seconds(start, Sys.time())

## 3. Vectorized code, local update on cluster centers

start <- Sys.time()
cluster_solutions[, "local-update"] <- fast_anticlustering(
  data,
  K = clusters
)
times["local-update"] <- get_seconds(start, Sys.time())


## 4. C implementation
start <- Sys.time()
cluster_solutions[, "C"] <- anticlustering(
  data,
  K = clusters,
  objective = "variance"
)
times["C"] <- get_seconds(start, Sys.time())

# Sekunden pro Methode:
round(times, 2)

# Faktor, relativiert an der C-Zeit: 
round(times / times["C"], 2)

# Test if all implementations have the same output:
all(cluster_solutions == cluster_solutions[, 1])
