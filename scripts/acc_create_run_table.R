# This script is to create a run table for producing the output for acc sims.
# A base table is constructed with all combinations of climate scenario
# and plgid to run. Country codes are added too. 
# Then a list of management params is created and merged with the base table.
# The management params are specified in acc_sims_prepare_init_settings.R 
# as is the man_scen to use.



# SOURCE_FILES -------------------------------------------------------------

source('scripts/settings.R')
source(config$PATH_acc_sims_prepare_init_settings)


# BASE-RUN-TABLE ---------------------------------------------------------------

print(paste0("Creating run table for man_scen: ", man_name))


country_codes_lookup <- get_acc_country_codes_lookup(aaa_all, country_codes)

# table_output_files <- grep("plgid", list.files(output_base_path), value = T)
table_output_files <- grep("plgid", list.files(clean_data_base_path), value = T)
plgid <- as.integer(unlist(tstrsplit(table_output_files, split = "_", keep = 2)))
clim_scen <- c("detrended", "gwl2", "gwl3", "gwl4")
man_scen <- man_name
model <- c("PREBAS")
# country <- c("Finland")
canopy_layer <- c(1)

acc_vectors_list <- list(plgid = plgid, 
                         clim_scen = clim_scen, 
                         man_scen = man_scen, 
                         model = model, 
                         # country = country, 
                         canopy_layer = canopy_layer)

acc_base_table <- expand_vectors_to_dt(acc_vectors_list)

acc_base_table[, model := as.character(model)]

# Add country codes col
acc_base_table_country <- merge(acc_base_table, 
                                country_codes_lookup[, c("PlgID", "country")], 
                                by.x = "plgid", by.y = "PlgID")



# MANAGEMENT-TABLE ---------------------------------------------------------



# Management params from acc settings
acc_man_vectors_list <- man_params[[man_name]]


acc_man_table <- rbindlist(lapply(clim_scen, function(cs) {
  create_table_from_vars(id_vars = list(clim_scen = cs, man_scen = man_scen), 
                         value_vars = acc_man_vectors_list, 
                         result_name = "man_init_args")
}))


acc_base_man_table <- merge.data.table(acc_base_table_country, acc_man_table, by = c("clim_scen", "man_scen"))


# STATIC-VARS-TABLE --------------------------------------------------------------


acc_static_vars_table <- data.table(varOutID = list(varOutID), vHarv = list(vHarv))
acc_static_vars_expanded_table <- rbindlist(replicate(nrow(acc_base_man_table), acc_static_vars_table, simplify = FALSE))




# RUN-TABLE ----------------------------------------------------------------

acc_run_table <- cbind(acc_base_man_table, acc_static_vars_expanded_table)



# SAVE --------------------------------------------------------------------


saveRDS(acc_run_table, run_table_full_path)






















