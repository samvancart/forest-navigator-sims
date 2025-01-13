source('scripts/settings.R')
source(config$PATH_acc_sims_prepare_init_settings)



plgid <- as.integer(7998833)
clim_scen <- "historical"
model <- "PREBAS"
country <- "Finland"
man_scen <- "noman"
canopy_layer <- 1



####management parameters
defaultThin = 0
ClCut = 0
mortMod = 3
ingrowth = T

acc_run_table <- data.table(plgid = plgid, clim_scen = clim_scen, 
                            model = model, country = country,
                            man_scen = man_scen, 
                            canopy_layer = canopy_layer,
                            man_init_args = list(list(defaultThin = defaultThin, 
                                                      ClCut = ClCut,
                                                      mortMod = mortMod,
                                                      ingrowth = ingrowth)),
                            
                            clean_data_base_path = clean_data_base_path,
                            selection_path = selection_path, 
                            clustered_base_path = clustered_base_path, 
                            aaa_file = aaa_file,
                            conversions_path = conversions_path,
                            output_base_path = output_base_path,
                            species_lookup = species_lookup,
                            varOutID = list(varOutID),
                            vHarv = list(vHarv)
                            )




produce_acc_output_obj_from_run_table <- function(acc_run_table) {
  
  output_objects <- apply(acc_run_table, 1, function(dt) {
    with(dt, {
      
      # Init model
      init_args <- list(plgid = plgid, clim_scen = clim_scen, clean_data_base_path = clean_data_base_path)
      initPrebas <- do.call(get_init_prebas_for_plgid, c(init_args, man_init_args))
      modOut <- get_modOut(regionPrebas, initPrebas)
      multiOut <- modOut$multiOut
      
      print(paste0("Getting siteID_lookup..."))
      # Get siteID lookup
      siteID_lookup <- get_siteID_lookup(plgid, selection_path, clustered_base_path, aaa_file)
      
      # Get conversions_dt
      conversions_dt <- fread(conversions_path)
      
      print(paste0("Creating output from multiOut..."))
      
      # Get multiOut as dt
      out_dt <- as.data.table(melt(multiOut[,,varOutID,,1]))
      out_dt_wide <- dcast.data.table(out_dt, site + year + layer ~ variable, value.var = "value")
      
      add_cols <- list(Model = model, Country = country, Climate_scenario = clim_scen, 
                       Management_scenario = man_scen, Canopy_layer = canopy_layer)
      # Get operations
      output_operations <- get_output_operations(plgid, multiOut, conversions_dt, siteID_lookup, species_lookup, add_cols)
      
      # Get output dt
      out_dt_melted <- transform_and_add_columns(out_dt_wide, output_operations)
      
      print("Done.")
      
      print("Creating acc output object...")
      
      output_template_vector <- c(model, plgid, clim_scen, man_scen)
      name <- str_c(output_template_vector, collapse = "_")
      
      # Get save path
      save_path <- get_acc_input_save_path(plgid, "output", output_base_path)
      
      list(name = name, plgid = plgid, data = out_dt_melted, save_path = save_path)
    })
  })
  
  print("Done.")
  
  return(output_objects)
}

output_dts <- produce_acc_output_obj_from_run_table(acc_run_table)






















































