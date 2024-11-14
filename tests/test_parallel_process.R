

test_that("get_in_parallel works correctly", {
  
  # Example data and function
  data_list <- list(data.frame(a = 1:10), data.frame(a = 11:20))
  my_fun <- function(df) sum(df$a)
  my_fun_args <- list()
  
  # Test basic functionality
  result <- get_in_parallel(data_list, my_fun, my_fun_args, cores = 1)
  expect_type(result, "list")
  expect_length(result, length(data_list))
  expect_equal(result, list(sum(1:10), sum(11:20)))
  
  # Test with multiple cores
  result_multi_cores <- get_in_parallel(data_list, my_fun, my_fun_args, cores = 2)
  expect_type(result_multi_cores, "list")
  expect_length(result_multi_cores, length(data_list))
  expect_equal(result_multi_cores, list(sum(1:10), sum(11:20)))
  
  # Test with additional arguments for foreach
  my_fun2 <- function(x) x * 2
  result_with_args <- get_in_parallel(data_list, my_fun2, my_fun_args, cores = 1, .combine = rbind)
  expect_type(result_with_args, "list")
  expect_length(result_with_args, 1)
  expect_equal(result_with_args, do.call(rbind, lapply(data_list, my_fun2)))
  
  
  # Test error handling
  expect_error(get_in_parallel(data_list, function(df) stop("error"), my_fun_args, cores = 1), "error")
  
  # Test input validations
  expect_error(get_in_parallel("not_a_list", my_fun, my_fun_args, cores = 1), "Assertion")
  expect_error(get_in_parallel(data_list, "not_a_function", my_fun_args, cores = 1), "Assertion")
  expect_error(get_in_parallel(data_list, my_fun, "not_a_list", cores = 1), "Assertion")
  expect_error(get_in_parallel(data_list, my_fun, my_fun_args, cores = -1), "Assertion")
  expect_error(get_in_parallel(data_list, my_fun, my_fun_args, cores = 1, type = "invalid_type"), "Assertion")
})

