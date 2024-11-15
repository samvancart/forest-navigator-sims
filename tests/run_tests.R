source('scripts/settings.R')
source("r/parallel_process.R")
source("r/clusters_dt.R")
source("r/utils.R")

test_file(config$TEST_clusters_dt)

test_file(config$TEST_parallel_process)

test_file(config$TEST_utils)



# Run all tests
test_dir(config$PATH_tests)
