

setwd("/home/martin/Seafile/Misc/anticlust_dependencies/")

all_needed <- tools::package_dependencies("anticlust", recursive = TRUE)

# suggested <- tools::package_dependencies("anticlust", which = "Suggests")
suggested <- list(anticlust = c("Rglpk", "Rsymphony", "tinytest"))

for (package in suggested) {
  all_needed <- c(all_needed, tools::package_dependencies(package, recursive = TRUE))
}

for (package in suggested) {
  all_needed <- c(all_needed, tools::package_dependencies(package, recursive = TRUE, which = "Suggests"))
}

all_neededv <- unname(unlist(all_needed))
download.packages(all_neededv, ".")
download.packages(unlist(suggested), ".")

# Removing rmarkdown as suggests/vignette builder removes most backage. Prior to removing
# all vignette packages: 24MB, after 8.9 MB
sapply(all_needed, sort)

# I still needed to remove testthat dependency
