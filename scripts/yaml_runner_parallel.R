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

temp_env$config_dt <- data.table(file_name = config_files)
temp_env$config_split_dt <- temp_env$split_dt_equal_with_constraint(temp_env$config_dt, 4, "file_name")
config_dts <- split(temp_env$config_split_dt, by = "splitID")

# Wrap scripts into functions
wrapped_scripts <- lapply(scripts, function(script) {
  script_str <- readLines(script)
  temp_env$wrap_script(script = script_str, param_names =  c("config_path", "parallel_run_id"))
})

rm(temp_env)




# Create a cluster with the desired number of cores
available_cores <- availableCores()
n_cores <- 4
cl <- makeCluster(n_cores)
registerDoParallel(cl)


max_id <- length(config_dts)
results <- list()
for(dt in config_dts[3:5]) {
  config_files <- dt$file_name
  id <- unique(dt$splitID)
  print(paste0("Processing ", id, " of ", max_id, "..."))
  
  
  t <- system.time({
    # Run the scripts in parallel for each config file
    result <- foreach(i = config_files, .combine = c, .packages = c("yaml")) %dopar% {
      
      # Run each script in wrapped_scripts consecutively with the same config data
      lapply(wrapped_scripts, function(script) {
        output <- capture.output({
          script(config_path = i)
        })
        output
      })
    }
  })

  results <- c(results, result)
  gc()
  print(t)
  print("Done.")
}

# Stop the cluster
stopCluster(cl)




























