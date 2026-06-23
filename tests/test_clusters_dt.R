
#  ------------------------ START TEST get_centers ------------------------ #

test_that("get_centers works correctly", {
  # Example data
  dt <- data.table(x = c(1, 2, 3, 4, 5), y = c(5, 4, 3, 2, 1))
  
  # Test basic functionality
  centers <- expect_no_warning(get_centers(dt))
  expect_type(centers, "integer")
  expect_gte(centers, 1)
  expect_lte(centers, 5)
  expect_equal(centers, 2)
  
  
  # Test with minimal unique rows
  dt_min <- data.table(x = c(1, 1), y = c(2, 2))
  centers_min <- expect_warning(get_centers(dt_min), regexp = "dt contains duplicate rows.")
  expect_equal(centers_min, 1)
  
  # Example data
  dt_2 <- data.table(x = c(1, 2, 3, 2, 1), y = c(5, 4, 3, 4, 5))
  
  # Test basic functionality
  centers <-expect_warning(get_centers(dt_2), regexp = "dt contains duplicate rows.")
  expect_type(centers, "integer")
  expect_gte(centers, 1)
  expect_lte(centers, 5)
  expect_equal(centers, 2)
  
  df <- data.frame(x = c(1, 4, 3, 4, 5), y = c(5, 2, 3, 2, 1))
  expect_error(get_centers(df), regexp = "Assertion on 'dt' failed: Must be a data.table, not data.frame.")
})


#  ------------------------ END TEST get_centers ------------------------ #

#  ------------------------ START TEST get_kmax ------------------------ #


test_that("get_kmax works correctly", {
  # Example data
  dt <- data.table(x = c(1, 2, 3, 4, 5), y = c(5, 4, 3, 2, 1))
  
  # Test basic functionality
  kmax <- get_kmax(dt)
  expect_type(kmax, "integer")
  expect_gte(kmax, 2)
  expect_equal(kmax, 4)
  
  # Test with minimal unique rows
  dt_min <- data.table(x = c(1, 1), y = c(2, 2))
  expect_type(kmax, "integer")
  kmax_min <- get_kmax(dt_min)
  expect_equal(kmax_min, 2)
  
  # Test with non-unique rows. The function does not check uniqueness of rows!
  dt_2 <- data.table(x = c(1, 4, 3, 4, 5), y = c(5, 2, 3, 2, 1))
  expect_type(kmax, "integer")
  kmax <- get_kmax(dt_2)
  expect_equal(kmax, 4)
  
  df <- data.frame(x = c(1, 4, 3, 4, 5), y = c(5, 2, 3, 2, 1))
  expect_error(get_kmax(df), regexp = "Assertion on 'dt' failed: Must be a data.table, not data.frame.")
})



#  ------------------------ END TEST get_kmax ------------------------ #





#  ------------------------ START TEST perform_clustering_by_group ------------------------ #


test_that("perform_clustering_by_group works correctly", {
  # Example data
  dt <- data.table(
    id = 1:20,
    group1 = rep(letters[1:2], each = 10),
    group2 = rep(letters[3:4], 10),
    x = runif(20),
    y = runif(20)
  )
  
  # Test basic functionality
  result <- expect_no_warning(perform_clustering_by_group(dt = dt, 
                                                          group_cols =  c("group1", "group2"), 
                                                          value_cols =  c("x", "y")))
  expect_s3_class(result, "data.table")
  expect_true("cluster_id" %in% colnames(result))
  expect_equal(nrow(result), 20)
  
  
  
  # Test with random seeds
  seeds <- sample(1:1000, 10, replace = TRUE)
  group_dts <- split(dt, by = c("group1", "group2"))
  
  for(seed in seeds) {
    expected_dt <- rbindlist(invisible(lapply(group_dts, function(group_dt) {
      set.seed = seed
      dt_subset <- group_dt[, c("x", "y")]
      centers <- get_centers(dt_subset)
      model <- kmeans(dt_subset, centers = centers)
      clusters <- model$cluster
      group_dt[, cluster_id := clusters]
    })))
    
    
    result_seed <- perform_clustering_by_group(dt, c("group1", "group2"), c("x", "y"), seed = seed)
    expect_s3_class(result_seed, "data.table")
    expect_true("cluster_id" %in% colnames(result_seed))
    expect_equal(nrow(result_seed), 20)
    expect_equal(result_seed, expected_dt)
  }
  
  
  # Test with non-unique dt
  set.seed <- 1
  
  dt$x[2:3] <- dt$x[1]
  dt$y[2:3] <- dt$y[1]

  result <- expect_warning(perform_clustering_by_group(dt = dt, 
                                                       group_cols =  c("group1", "group2"), 
                                                       value_cols =  c("x", "y"))
                           , regexp = "dt contains duplicate rows.")
  expect_s3_class(result, "data.table")
  expect_true("cluster_id" %in% colnames(result))
  expect_equal(nrow(result), 20)
  
  
  # Test input validations
  expect_error(perform_clustering_by_group("not_a_data_table", c("group1"), c("x")), regexp =  "Assertion on 'dt' failed: Must be a data.table, not character.")
  expect_error(perform_clustering_by_group(dt, "not_a_character_vector", c("x")), regexp =  "object 'not_a_character_vector' not found")
  expect_error(perform_clustering_by_group(dt, c("group1")), regexp =  "argument \"value_cols\" is missing, with no default")
  expect_error(perform_clustering_by_group(dt, c("group1"), c("x"), seed = "not_an_integer"), regexp =  "Assertion on 'seed' failed: Must be of type 'single integerish value' \\(or 'NULL'\\), not 'character'.")
})



#  ------------------------ END TEST perform_clustering_by_group ------------------------ #











