source('scripts/settings.R')
source("r/parallel_process.R")
source("r/clusters_dt.R")
source("r/utils.R")
source("r/acc_sims.R")




test_file(file.path(config$PATH_tests, "test_clusters_dt.R"))

test_file(file.path(config$PATH_tests, "test_parallel_process.R"))

test_file(file.path(config$PATH_tests, "test_acc_sims.R"))



# Run all tests
test_dir(config$PATH_tests)
