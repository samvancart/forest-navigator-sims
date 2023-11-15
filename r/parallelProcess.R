
# Functions

# Use parallel processing to run a function
# Params:
# data (list): List of inputs to process eg. data frames or vectors.
# fun (function): Function to be run in parallel. Must have one required parameter which has the same class as an object in data.
# libs (list): List of libraries required to run input function.
# sources (list): List of source files required to run input function.
# fun_kwargs (list): List of additional arguments or keyword arguments required by input function.
# Returns:
# Results as list.

get_in_parallel <- function(data, fun, libs = list(), sources = list(), fun_kwargs = list()){
  
  print(paste0("Parallel processing..."))
  
  # Parallel process file
  cl <- makeCluster(length(data), type="SOCK")
  registerDoParallel(cl)
  t <- system.time(
    result <- foreach(df = data) %dopar% {
      # Load libraries
      lapply(libs, require, character.only=T)
      
      # Source files
      lapply(sources, source)

      # Call function
      do.call(fun, c(list(df), fun_kwargs))
    }
  )
  
  print("Done.")
  print(t)
  
  # Remember to stop cluster!
  on.exit(parallel::stopCluster(cl))
  
  return(result)
  
}







