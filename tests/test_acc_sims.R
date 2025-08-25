


# TEST treeData_worker ---------------------------------------

# sample_until_global_threshold
test_that("sample_until_global_threshold works correctly with random seeds and thresholds", {
  # Create example data
  dt <- data.table(id = 1:10, value = runif(10))
  
  # Generate random seeds and thresholds
  set.seed(NULL) # Set seed for reproducibility of the test itself
  seeds <- sample(1:1000, 10) # Generate 5 random seeds
  thresholds <- sample(seq(0.5, 5, by = 0.5), 10) # Generate 5 random thresholds between 0.5 and 5
  
  for (seed in seeds) {
    for (threshold in thresholds) {
      # Test with varying seeds and thresholds
      result <- sample_until_global_threshold(dt, "value", threshold, seed = seed)
      expect_s3_class(result, "data.table")
      expect_true("cum_sum" %in% colnames(result))
      expect_lte(sum(result$value), threshold) # Check if the cumulative sum is <= threshold
      
      gte_val <- 0
      if(nrow(result) > 1) {
        gte_val <- result[nrow(result)-1, ]$cum_sum
      }
      
      expect_gte(sum(result$value), gte_val)
      
      # Test reproducibility with the same seed and threshold
      result1 <- sample_until_global_threshold(dt, "value", threshold, seed = seed)
      result2 <- sample_until_global_threshold(dt, "value", threshold, seed = seed)
      expect_equal(result1, result2) # Check if results are identical with the same seed
    }
  }
  
  # Test input validations
  expect_error(sample_until_global_threshold("not_a_data_table", "value", 3), regexp = "Assertion on 'dt' failed: Must be a data.table, not character.")
  expect_error(sample_until_global_threshold(dt, 123, 3), regexp = "Assertion on 'value_col' failed: Must be of type 'string', not 'double'.")
  expect_error(sample_until_global_threshold(dt, "non_existent_col", 3), regexp = "Assertion on 'value_col' failed: Names must be a subset of \\{'id','value'\\}, but has additional elements \\{'non_existent_col'\\}.")
  expect_error(sample_until_global_threshold(dt, "value", "not_a_number"), regexp = "Assertion on 'threshold' failed: Must be of type 'number', not 'character'.")
})


# TEST dClass_worker ------------------------------------------------------

# d_class_assign_classes
test_that("Assigns classes correctly with given d_class and max_d_class", {
  # Create test data
  test_data <- data.table(D = c(2, 5, 10, 15, 20))
  
  result <- d_class_assign_classes(test_data, d_class = 5, max_d_class = 25)
  
  expected <- factor(x = c("0_5", "5_10", "10_15", "15_20", "20_25"), levels = c("0_5", "5_10", "10_15", "15_20", "20_25"))
  
  expect_true("class" %in% colnames(result))
  expect_equal(result$class, expected)
})

test_that("Handles NA values in D", {
  test_data_na <- data.table(D = c(2, NA, 10, 15, 20))
  
  result <- d_class_assign_classes(test_data_na, d_class = 5, max_d_class = 25)
  
  expected <- factor(x = c("0_5", NA, "10_15", "15_20", "20_25"), levels = c("0_5", "5_10", "10_15", "15_20", "20_25"))

  expect_true("class" %in% colnames(result))
  expect_equal(result$class, expected)
})

test_that("Handles edge cases for D = -1", {
  test_data_zero <- data.table(D = c(-1, 5, 10))
  result <- d_class_assign_classes(test_data_zero, d_class = 5, max_d_class = 15)

  expected <- factor(x = c(NA, "5_10", "10_15"), levels = c("0_5", "5_10", "10_15"))
  
  expect_equal(result$class, expected)
})

test_that("Handles max_d_class limiting correctly", {
  test_data_limit <- data.table(D = c(5, 10, 15, 20, 25))
  result <- d_class_assign_classes(test_data_limit, d_class = 5, max_d_class = 20)
  
  expected <- factor(x = c("5_10", "10_15", "15_20", NA, NA), levels = c("0_5", "5_10", "10_15", "15_20"))
  
  expect_equal(result$class, expected) # Last value constrained
})



# d_class_get_dcast_dt
test_that("Dcast correctly groups site, year, and species", {
  test_data <- data.table(
    site = c(1, 2, 1, 2, 1),
    year = c(2020, 2020, 2021, 2021, 2021),
    species = c(101, 202, 101, 202, 303),
    class = c("0_5", "5_10", "10_15", "15_20", ">20"),
    Nclass = c(3, 5, 7, 2, 1)
  )
  
  result <- d_class_get_dcast_dt(test_data, d_class = 5, max_d_class = 20)
  
  expect_true(all(c("site", "year", "species") %in% names(result)))
  expect_true(all(c("0_5", "5_10", "10_15", "15_20", ">20") %in% names(result)))
})

test_that("Missing d_class labels are correctly initialized to 0", {
  test_data <- data.table(
    site = c(1, 2),
    year = c(2020, 2020),
    species = c(101, 202),
    class = c("0_5", "5_10"),
    Nclass = c(3, 5)
  )
  
  result <- d_class_get_dcast_dt(test_data, d_class = 5, max_d_class = 20)
  
  expect_equal(result$`10_15`, c(0, 0))
  expect_equal(result$`15_20`, c(0, 0))
  expect_equal(result$`>20`, c(0, 0))
})

test_that("Values are correctly assigned to class columns", {
  test_data <- data.table(
    site = c(1, 2, 1, 2, 1),
    year = c(2020, 2020, 2021, 2021, 2021),
    species = c(101, 202, 101, 202, 303),
    class = c("0_5", "5_10", "10_15", "15_20", ">20"),
    Nclass = c(3, 5, 7, 2, 1)
  )
  
  result <- d_class_get_dcast_dt(test_data, d_class = 5, max_d_class = 20)
  
  expected_values <- data.table(
    site = c(1, 1, 1, 2, 2),
    year = c(2020, 2021, 2021, 2020, 2021),
    species = c(101, 101, 303, 202, 202),
    `0_5` = c(3, 0, 0, 0, 0),
    `5_10` = c(0, 0, 0, 5, 0),
    `10_15` = c(0, 7, 0, 0, 0),
    `15_20` = c(0, 0, 0, 0, 2),
    `>20` = c(0, 0, 1, 0, 0)
  )
  
  expect_equal(result[, .SD, .SDcols = names(expected_values)], expected_values)
})

test_that("Column ordering is maintained as expected", {
  test_data <- data.table(
    site = c(1, 2, 1, 2, 1),
    year = c(2020, 2020, 2021, 2021, 2021),
    species = c(101, 202, 101, 202, 303),
    class = c("0_5", "5_10", "10_15", "15_20", ">20"),
    Nclass = c(3, 5, 7, 2, 1)
  )
  
  result <- d_class_get_dcast_dt(test_data, d_class = 5, max_d_class = 20)
  
  expected_order <- c("site", "year", "species", "0_5", "5_10", "10_15", "15_20", ">20")
  expect_equal(names(result), expected_order)
})


























