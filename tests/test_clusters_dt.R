
#  ------------------------ START TEST get_centers ------------------------ #

test_that("get_centers works correctly", {
  # Example data
  dt <- data.table(x = c(1, 2, 3, 4, 5), y = c(5, 4, 3, 2, 1))
  
  # Test basic functionality
  centers <- get_centers(dt, kmax = 4)
  expect_type(centers, "integer")
  expect_gte(centers, 1)
  expect_lte(centers, 5)
  
  # Test with minimal unique rows
  dt_min <- data.table(x = c(1, 1), y = c(2, 2))
  centers_min <- get_centers(dt_min, kmax = 2)
  expect_equal(centers_min, 1)
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
  
  # Test with minimal unique rows
  dt_min <- data.table(x = c(1, 1), y = c(2, 2))
  kmax_min <- get_kmax(dt_min)
  expect_equal(kmax_min, 2)
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
  result <- perform_clustering_by_group(dt = dt, group_cols =  c("group1", "group2"), value_cols =  c("x", "y"))
  expect_s3_class(result, "data.table")
  expect_true("cluster_id" %in% colnames(result))
  expect_equal(nrow(result), 20)
  
  # Test with seed
  result_seed <- perform_clustering_by_group(dt, c("group1", "group2"), c("x", "y"), seed = 123)
  expect_s3_class(result_seed, "data.table")
  expect_true("cluster_id" %in% colnames(result_seed))
  expect_equal(nrow(result_seed), 20)
  
  # Test input validations
  expect_error(perform_clustering_by_group("not_a_data_table", c("group1"), c("x")), "Assertion")
  expect_error(perform_clustering_by_group(dt, "not_a_character_vector", c("x")), "Assertion")
  expect_error(perform_clustering_by_group(dt, c("group1"), "not_a_character_vector"), "Assertion")
  expect_error(perform_clustering_by_group(dt, c("group1"), c("x"), seed = "not_an_integer"), "Assertion")
})



#  ------------------------ END TEST perform_clustering_by_group ------------------------ #











