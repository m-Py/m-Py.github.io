
library(anticlust)

source("./rsource/27_Slow_Kmeans.R")

N <- 100
M <- 2
K <- 2
data <- matrix(rnorm(N * M), ncol = M)
clusters <- sample(rep_len(1:K, N))

methods <- c(
  "non-vectorized", 
  "vectorized-1", 
  "vectorized-2", 
  "local-update",
  "C"
)

times <- rep(NA, length(methods))
names(times) <- methods
cluster_solutions <- matrix(NA, ncol = length(methods), nrow = N)
colnames(cluster_solutions) <- methods

## 1. Using non-vectorized code
start <- Sys.time()
cluster_solutions[, 1]  <- anticlustering(
  data,
  K = clusters,
  objective = variance_objective_slow
)
times[1] <- difftime(Sys.time(), start, units = "secs")

## 2. Using vectorized code, each iteration includes input validation
vectorized1 <- function(clusters, data) {
  variance_objective(data, clusters)
}

start <- Sys.time()
cluster_solutions[, 2] <- anticlustering(
  data,
  K = clusters,
  objective = vectorized1
)
times[2] <- difftime(Sys.time(), start, units = "secs")

## 3. Vectorized code, no input validation in objective function
vectorized2 <- anticlust:::variance_objective_

start <- Sys.time()
cluster_solutions[, 3] <- anticlustering(
  data,
  K = clusters,
  objective = vectorized2
)
times[3] <- difftime(Sys.time(), start, units = "secs")


## 4. Vectorized code, local update on cluster centers

start <- Sys.time()
cluster_solutions[, 4] <- anticlustering(
  data,
  K = clusters,
  objective = "variance"
)
times[4] <- difftime(Sys.time(), start, units = "secs")


## 5. C implementation
start <- Sys.time()
cluster_solutions[, 5] <- fanticlust(data, clusters)
times[5] <- difftime(Sys.time(), start, units = "secs")

# Sekunden pro Methode:
round(times, 2)

# Faktor, relativiert an der C-Zeit: 
round(times / times["C"], 2)

# Test if all implementations have the same output:
all(cluster_solutions == cluster_solutions[, 1])
