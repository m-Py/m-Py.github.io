

# source this file to get the first naive implementation of the k-means objective
# in the `anticlust` package. `variance_objective_slow` can then be passed
# to the `objective` argument in the `anticlustering()` function.

variance_objective_slow <- function(data, clusters) {
  ## 1. Compute cluster centers
  centers <- cluster_centers(data, clusters)
  ## 2. For each item, compute distance to each cluster center
  distances <- dist_from_centers(data, centers)
  ## 3. Use two-column matrix to select relevant distances
  distances <- distances[cbind(1:nrow(distances), clusters)]
  sum(distances)
}

#' Compute cluster centers
#'
#' @param features A data matrix of element features
#' @param clusters A numeric vector indicating cluster membership
#'   of each element
#'
#' @return A matrix of cluster centers. Rows represent clusters and
#'   columns represent features

cluster_centers <- function(features, clusters) {
  features <- as.matrix(features) #  if features is a vector
  centers <- by(features, clusters, colMeans)
  do.call(rbind, as.list(centers)) # as.list for the case of only one feature
}

# Determine distances of n data points to m cluster centers
#
# The data basis for the clustering algorithm implemented in function
# `equal_sized_clustering`.
#
# @param features A vector, matrix or data.frame of data points. If a
#     matrix or data.frame is passed, rows correspond to items and
#     columns to features.
# @param centers A matrix of cluster centers. Each row corresponds to a
#     cluster and each column corresponds to a feature (this format is,
#     for example, returned by the function `stats::kmeans` through the
#     element `centers`).
#
# @return A data matrix; columns represent clusters
#     and contain the distance to the respective cluster for each item.
#
dist_from_centers <- function(features, centers) {
  features <- as.matrix(features) # if points is only a vector
  ## store all distances from centers
  storage <- matrix(ncol = nrow(centers), nrow = nrow(features))
  ## determine distances from all cluster centers:
  for (i in 1:ncol(storage)) {
    storage[, i] <- dist_one_center(features, centers[i, ])
  }
  return(storage)
}

## compute the distances of a vector (or matrix or data.frame) of points
## to a cluster center. points has the same form as in the function
## above.
dist_one_center <- function(points, center) {
  distances <- vector(length = nrow(points))
  for (i in 1:nrow(points)) {
    distances[i] <- euc_dist2(points[i, ], center)
  }
  return(distances)
}


## A squared euclidian distance between two data points
euc_dist2 <- function(x1, x2) {
  sum((x1 - x2)^2)
}
