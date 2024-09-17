# This script is for running scripts using parallel processing. The script reads
# temporary yaml configuration files that contain predetermined settings.



# Load libs and config
source('scripts/settings.R')

# Load run table
source("scripts/define_vars_for_runner.R")

# Get run_table column names
id_names <- names(run_table_dt)[which(!names(run_table_dt) %in% "src")]

# Define paths
source_file <- config_path
destination_dir <- "temp"

# Copy config files and modify ids based on run table
invisible(lapply(1:nrow(run_table_dt), function(i) {
  config_file_name <- paste0("config_", i, ".yaml")
  destination_file <- file.path(destination_dir, config_file_name)
  file.copy(source_file, destination_file)
  cat("\n")
  print(paste0("Copied ", source_file, " into ", destination_file))
  cat("\n")
  ids <- unlist(run_table_dt[i, ..id_names])
  modify_yaml_settings_vector(destination_file, ids)
}))

# List of config files and scripts
config_files <- list.files("temp", full.names = T)
scripts <- as.list(run_table_dt$src[[1]])


temp_env <- new.env()
source("r/utils.R", temp_env)

# Wrap scripts into functions
wrapped_scripts <- lapply(scripts, function(script) {
  script_str <- readLines(script)
  temp_env$wrap_script(script_str, c("config_path"))
})

rm(temp_env)




# Create a cluster with the desired number of cores
available_cores <- availableCores()
n_cores <- 8
cl <- makeCluster(n_cores)
registerDoParallel(cl)

t <- system.time({
  # Run the scripts in parallel for each config file
  results <- foreach(i = config_files[1:8], .combine = c, .packages = c("yaml")) %dopar% {
    
    # Run each script consecutively with the same config data
    lapply(wrapped_scripts, function(script) {
      output <- capture.output({
        script(config_path = i)
      })
      output
    })
  }
})

# Stop the cluster
stopCluster(cl)


print(t)
























