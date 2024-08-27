source('scripts/settings.R')
source('./r/utils.R')






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











