source('scripts/settings.R')
source(config$PATH_acc_sims_prepare_init_settings)



plgid <- as.integer(7998833)
clim_scen <- "historical"


####management parameters
defaultThin = 0
ClCut = 0
mortMod = 3
ingrowth = T

init_args <- list(plgid = plgid, clim_scen = clim_scen, clean_data_base_path = clean_data_base_path)
man_init_args <- list(defaultThin = defaultThin,
                      ClCut = ClCut,
                      mortMod = mortMod,
                      ingrowth = ingrowth)

initPrebas <- do.call(get_init_prebas_for_plgid, c(init_args, man_init_args))
modOut <- get_modOut(regionPrebas, initPrebas)
multiOut <- modOut$multiOut


# Get siteID lookup
selection_path <- "data/acc/input/test_sites/raw/grid/filtered_selection_fi_cell10.csv"
clustered_base_path <- paste0("data/acc/input/", simulation_site, "/raw/clustered")
siteID_lookup <- get_siteID_lookup(plgid, selection_path, clustered_base_path, aaa_file)


#### foroutput processing
varOutID <- c(44,18,19,11:13,17,30,43,42,37,7,22,31:33,24:25,47,50)
vHarv <- c(30,2)

out_dt_all <- get_melted_multiOut_dt_with_species_and_harv(plgid, multiOut, varOutID)
out_dt_all <- merge(out_dt_all, siteID_lookup, by = c("site"))



#!!!!!issues: 
# PAR units is wrong
# first column seems to be an ID 
# are the leap years processed correctly?
# 
# parTran[,1]
# 
# parTran <- parTran[,2:26585]*100  
# vpdTran <- vpdTran[,2:26585]
# co2Tran <- co2Tran[,2:26585]
# precipTran <- precipTran[,2:26585]
# tairTran <- tairTran[,2:26585]




# harv  ###check over bark  and tree tops are included?
#transp ####we have ET not just transp
###branch biomass?separately or included in the stem?
stem_biom_sap
stem_biom_heart







conversions_dt <- fread("data/acc/docs/forest_nav_conversions_lookup.csv")



varNames
unique(out_dt_all$variable)
unique(conversions_dt$Variable)

add_columns_to_dt <- function(dt, columns) {
  # Ensure columns is a named list
  assert_list(columns, names = "named")
  
  # Add columns to the data.table
  dt[, names(columns) := mget(names(columns), envir = as.environment(columns))]
  
  return(dt)
}

# Add columns from table
add_single_row_columns <- function(base_dt, info_dt) {
  # Ensure info_dt has only one row
  assert_true(nrow(info_dt) == 1)
  
  # Replicate the values from info_dt to match the number of rows in base_dt
  repeated_info_dt <- info_dt[rep(1, nrow(base_dt)), ]
  
  # Combine the base data.table with the repeated info data.table
  combined_dt <- cbind(base_dt, repeated_info_dt)
  
  return(combined_dt)
}



add_cols <- list(Model = "PREBAS", Country = "Finland", Climate_scenario = clim_scen, Management_scenario = "noman")
out_dt_all <- add_columns_to_dt(out_dt_all, add_cols)


out_dt_all[, Model := "PREBAS"]
out_dt_all[, Country := "Finland"]
out_dt_all[, Climate_scenario := clim_scen]
out_dt_all[, Management_scenario := "noman"]











































