source("r/utils.R")

library(yaml)
library(stringr)


# Path to config file
config_path <- paste0("config.yaml")

# Load the configuration
config <<- yaml.load_file(config_path)



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


script <- scripts[16]
str <- "nfi_sweden_paths"
pattern <- paste0("\\b", str, "\\b")
replacement_pattern <- "VAR_nfi_sweden_paths"



old_strs <- names(config)
patterns <- unlist(lapply(old_strs, function(str) paste0("\\b", str, "\\b")))

replacements <- unlist(lapply(old_strs, function(str) paste0("config$", str)))





# REPLACE
invisible(lapply(filtered_scripts, function(script){
  lapply(seq_along(patterns), function(i) {
    replace_in_script(script, patterns[i], replacements[i], T)
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











