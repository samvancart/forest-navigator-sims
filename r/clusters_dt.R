

#' Optimal number of cluster centers
#'
#' @param dt A data.table object containing the data.
#' @param kmax An integer specifying the maximum number of clusters.
#' @return An integer indicating the optimal number of cluster centers.
#' @importFrom checkmate assert_data_table assert_int
get_centers <- function(dt, kmax) {
  # Input validations
  assert_data_table(dt)
  assert_int(kmax, lower = 2)
  
  centers <- 1
  unique_rows <- unique(dt)

  
  if (nrow(unique_rows) > 2) {
    optimal_num_clusters <- fviz_nbclust(unique_rows, kmeans, method = "silhouette", k.max = kmax)
    centers <- which.max(optimal_num_clusters$data$y)
  }
  return(centers)
}


#' Kmax for optimizing number of clusters
#'
#' @param dt A data.table object containing the data.
#' @return An integer indicating the maximum number of clusters.
#' @importFrom checkmate assert_data_table
get_kmax <- function(dt) {
  # Input validations
  assert_data_table(dt)
  
  unique_rows <- unique(dt)
  
  if (nrow(unique_rows) > 2) {
    kmax <- nrow(unique_rows) - 1
  } else {
    kmax <- 2
  }
  return(as.integer(kmax))
}


#' Perform clustering by groups and assign cluster IDs with additional arguments
#'
#' @description This function performs K-means clustering on specified value columns within each group of a data.table and assigns cluster IDs to the data.
#'
#' @param dt A `data.table` containing the data.
#' @param group_cols A character vector specifying the columns to group by.
#' @param value_cols A character vector specifying the columns to use for clustering.
#' @param seed An optional integer to set the seed for reproducibility. Defaults to `NULL`.
#' @param ... Additional arguments to be passed to the `kmeans` function.
#'
#' @return The input `data.table` with an added column `cluster_id` indicating the cluster assignments.
#'
#' @import data.table
#' @import checkmate
#' @importFrom stats kmeans
#' @examples
#' library(data.table)
#' dt <- data.table(
#'   id = 1:20,
#'   group1 = rep(letters[1:2], each = 10),
#'   group2 = rep(letters[3:4], 10),
#'   x = runif(20),
#'   y = runif(20)
#' )
#' result <- perform_clustering_by_group(dt, c("group1", "group2"), c("x", "y"), seed = 123, nstart = 25, iter.max = 50)
#' print(result)
#' @export
perform_clustering_by_group <- function(dt, group_cols, value_cols, seed = NULL, ...) {
  # Input validations
  assert_data_table(dt)
  assert_character(group_cols, min.len = 1)
  assert_character(value_cols, min.len = 1)
  assert_int(seed, null.ok = T)
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  dt[, {
    dt_subset <- .SD[, ..value_cols]
    
    kmax <- get_kmax(dt_subset)
    centers <- get_centers(dt_subset, kmax)
    
    # Perform K-means clustering with additional arguments passed through ...
    model <- kmeans(dt_subset, centers = centers, ...)
    
    cluster_ids <- model$cluster
    
    dt <- .SD
    
    dt$cluster_id <- cluster_ids
    
    dt
    
  }, by = group_cols]
}





