source('scripts/settings.R')
source(config$PATH_acc_sims_prepare_init_settings)









expand_vectors_to_dt <- function(vectors_list) {
  # Create expanded grid
  expanded_grid <- expand.grid(vectors_list)
  
  # Convert to data.table
  result_dt <- as.data.table(expanded_grid)
  
  # Set column names
  setnames(result_dt, names(vectors_list))
  
  return(result_dt)
}



create_table_from_vars <- function(id_vars, value_vars_list, result_name = "result") {
  # Check input validity
  assert_list(id_vars)
  assert_true(all(vapply(id_vars, is.vector, logical(1))))
  assert_list(value_vars_list)
  assert_true(all(vapply(value_vars_list, length, integer(1)) == 1))
  assert_character(result_name, len = 1)
  
  # Create a data.table with id_vars
  dt <- as.data.table(id_vars)
  
  # Combine value_vars into a single list
  combined_value_vars <- list(value_vars_list)
  
  # Add the combined value_vars list as a new column
  dt[, (result_name) := combined_value_vars]
  
  return(dt)
}




table_output_files <- grep("plgid", list.files(output_base_path), value = T)
plgid <- as.integer(unlist(tstrsplit(table_output_files, split = "_", keep = 2)))
clim_scen <- c("detrended", "gwl2", "gwl3", "gwl4")
man_scen <- c("noman")
model <- c("PREBAS")
country <- c("Finland")
canopy_layer <- c(1)

acc_vectors_list <- list(plgid = plgid, 
                         clim_scen = clim_scen, 
                         man_scen = man_scen, 
                         model = model, 
                         country = country, 
                         canopy_layer = canopy_layer)

acc_base_table <- expand_vectors_to_dt(acc_vectors_list)


####management parameters
defaultThin = 0
ClCut = 0
mortMod = 3
ingrowth = T
acc_man_noharv_historical_vectors_list <- list(defaultThin = 0, 
                                               ClCut = 0, 
                                               mortMod = 3, 
                                               ingrowth = T)


acc_man_table <- rbindlist(lapply(clim_scen, function(cs) {
  create_table_from_vars(id_vars = list(clim_scen = cs, man_scen = man_scen), 
                         value_vars = acc_man_noharv_historical_vectors_list, 
                         result_name = "man_init_args")
}))

# acc_man_table <- create_table_from_vars(id_vars = list(clim_scen = clim_scen, man_scen = man_scen), 
#                                         value_vars = acc_man_noharv_historical_vectors_list, 
#                                         result_name = "man_init_args")



acc_base_man_table <- merge.data.table(acc_base_table, acc_man_table, by = c("clim_scen", "man_scen"))



acc_static_vars_table <- data.table(clean_data_base_path = clean_data_base_path,
                                    selection_path = selection_path, 
                                    clustered_base_path = clustered_base_path, 
                                    aaa_file = aaa_file,
                                    conversions_path = conversions_path,
                                    output_base_path = output_base_path,
                                    species_lookup_path = species_lookup_path,
                                    varOutID = list(varOutID),
                                    vHarv = list(vHarv))

acc_static_vars_expanded_table <- rbindlist(replicate(nrow(acc_base_man_table), acc_static_vars_table, simplify = FALSE))



acc_run_table <- cbind(acc_base_man_table, acc_static_vars_expanded_table)



























