source("r/utils.R")

library(yaml)
library(stringr)


# Path to config file
config_path <- paste0("config.yaml")

# Load the configuration
config <<- yaml.load_file(config_path)

# Dynamically assign variables based on list names in the config
for (list_name in names(config)) {
  assign(list_name, config[[list_name]], envir = .GlobalEnv)
}


# Define a function to update the script and store modified line numbers
update_script_from_config <- function(file_path, config) {
  script <- readLines(file_path)
  modified_lines <- list()
  
  # Iterate over each list in the config
  lapply(names(config), function(list_name) {
    list_content <- config[[list_name]]
    
    # Replace occurrences of each object in the list
    lapply(names(list_content), function(object_name) {
      pattern <- paste0("\\b", object_name, "\\b")
      replacement <- paste0(list_name, "$", object_name)
      
      # Track modified lines
      modified <- grepl(pattern, script)
      script[modified] <<- gsub(pattern, replacement, script[modified])
      modified_lines <<- c(modified_lines, which(modified))
    })
  })
  
  # writeLines(script, file_path)
  modified_lines_vector <- sort(unlist(modified_lines))
  print(script[unique(modified_lines_vector)])
  return(modified_lines_vector)
}




count_occurences_of_pattern_in_script <- function(file_path, pattern) {
  script <- readLines(file_path)
  counts <- str_count(script, pattern)
  total <- sum(counts)
  lines <- which(counts>0)
  line_counts <- counts[which(counts>0)]
  return(list(file_path = file_path, 
                counts = counts, total = total, 
                lines = lines, 
                line_counts = line_counts))
}

replace_in_script <- function(file_path, pattern, replacement, test=F) {
  # Read the script from the file
  script <- readLines(file_path)
  
  # Replace all occurrences of the pattern with the replacement
  mod_script <- gsub(pattern, replacement, script)
  
  # Write the modified script back to the file
  
  if(!setequal(script, mod_script)) {
    if(!test) {
      print(paste0("Modifying ", file_path, ": Pattern is ", pattern, " and replacement is ", replacement))
      writeLines(mod_script, file_path)
    } else {
      print(paste0("TEST: Modifying ", file_path, ": Pattern is ", pattern, " and replacement is ", replacement))
    }

  }
  
  
 
}


scripts <- list.files("scripts", full.names = T)
filtered_scripts <- scripts[!scripts %in% c("scripts/run.R", "scripts/settings.R", "scripts/modify_script_lines.R")]

# MODIFY
modified_lines_vector <- update_script_from_config(scripts[15], config)
length(unique(unlist(modified_lines_vector)))


script <- scripts[16]
str <- "nfi_sweden_paths"
pattern <- paste0("\\b", str, "\\b")
replacement_pattern <- "VAR_nfi_sweden_paths"



old_strs <- c(
  "soilData_path",
  "tranPath",
  "historical_climate_data_gitlab_path",
  "prebas_gitlab_path",
  "prebas_future_GFDL_ESM4_SSP370",
  "prebas_future_UKESM1_0_LL_ssp370",
  "prebas_historical_detrend",
  "prebas_eobs_path",
  "nfi_sweden_path",
  "rdata_path",
  "chelsa_file",
  "chelsa_path",
  "eobs_file",
  "eobs_path",
  "eobs_var",
  "eobs_folder",
  "sites_path"
)
patterns <- unlist(lapply(old_strs, function(str) paste0("\\b", str, "\\b")))

replacements <- c(
  "PATH_soil_data",
  "PATH_tran",
  "PATH_historical_climate_data_gitlab",
  "PATH_prebas_gitlab",
  "PATH_prebas_future_GFDL_ESM4_SSP370",
  "PATH_prebas_future_UKESM1_0_LL_ssp370",
  "PATH_prebas_historical_detrend",
  "PATH_prebas_eobs",
  "PATH_nfi_sweden",
  "PATH_rdata",
  "PATH_chelsa_file_nc",
  "PATH_chelsa_nc",
  "PATH_eobs_file_nc",
  "PATH_eobs_nc",
  "PATH_eobs_var_nc",
  "PATH_eobs_folder_nc",
  "PATH_sites"
)





# REPLACE
invisible(lapply(filtered_scripts, function(script){
  lapply(seq_along(patterns), function(i) {
    replace_in_script(script, patterns[i], replacements[i], F)
  })
}))

invisible(lapply(filtered_scripts, function(x) replace_in_script(x, pattern, replacement_pattern, T)))

# replace_in_script(script, pattern, "VAR_species_id")




# COUNTS
counts_info_list <- count_occurences_of_pattern_in_script(script, pattern)


invisible(lapply(filtered_scripts, function(script) {
  counts <- count_occurences_of_pattern_in_script(script, pattern)
  
  if(counts$total>0) {
    print(paste0("Script: ", script))
    print(paste0("str: ", str))
    print(paste0("Total occurences: ", counts$total))
    invisible(lapply(seq_along(counts$lines), function(i) {
      print(paste0("Line ", counts$lines[i], ": ", counts$line_counts[i]))
      print(readLines(script)[counts$lines[i]])
    }))
    cat("\n")
  }
  
}))











