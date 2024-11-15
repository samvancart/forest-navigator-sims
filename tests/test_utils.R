

#  ------------------------ START TEST sample_until_global_threshold ------------------------ #

# Unittests
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




#  ------------------------ END TEST sample_until_global_threshold ------------------------ #