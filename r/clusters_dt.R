

#' Optimal number of cluster centers
#'
#' @param dt A data.table object containing the data.
#' @return An integer indicating the optimal number of cluster centers.
#' @importFrom checkmate assert_data_table
get_centers <- function(dt) {
  
  # Input validations
  assert_data_table(dt)
  
  centers <- 1
  unique_rows <- unique(dt)
  
  if(nrow(unique_rows) < nrow(dt)) {
    warning("dt contains duplicate rows.")
  }
  
  kmax <- get_kmax(unique_rows)
  
  if (nrow(unique_rows) > 2) {
    optimal_num_clusters <- fviz_nbclust(unique_rows, kmeans, method = "silhouette", k.max = kmax)
    centers <- which.max(optimal_num_clusters$data$y)
  }
  return(as.integer(centers))
}


#' Kmax for optimizing number of clusters
#'
#' @param dt A data.table object containing the data. Rows should be unique but no check is performed here!
#' @return An integer indicating the maximum number of clusters.
#' @importFrom checkmate assert_data_table
get_kmax <- function(dt) {
  
  # Input validations
  assert_data_table(dt)
  
  max_rows <- nrow(dt)-1
  kmax <- as.integer(max(2, max_rows))
  
  return(kmax)
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
  assert_int(seed, null.ok = TRUE)
  
  # Original dt column order
  col_order <- colnames(dt)
  
  # Set seed for reproducibility. If seed is NULL no seed is set.
  set.seed(seed)
  
  clusters_dt <- dt[, {
    # Filter dt by value_cols
    dt_subset <- .SD[, mget(value_cols)]
    
    # Get cluster centres
    centers <- get_centers(dt_subset)
    
    # Perform K-means clustering with additional arguments passed through ...
    model <- kmeans(dt_subset, centers = centers, ...)
    
    # Get the clusters
    cluster_ids <- model$cluster
    
    dt <- .SD
    
    dt$cluster_id <- cluster_ids
    
    dt
    
  }, by = group_cols]
  
  setcolorder(clusters_dt, neworder =  c(col_order, "cluster_id"))
}





