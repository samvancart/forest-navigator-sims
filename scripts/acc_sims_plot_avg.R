# Plot yearly averages for sums of variables by layer and species.

# SOURCE_FILES -------------------------------------------------------------

source('scripts/settings.R')
source(config$PATH_acc_sims_prepare_init_settings)


# GET_FILES_TO_PLOT -----------------------------------------------------------


bucket_list_prefix <- file.path("output", simulation_site, "output_files")

# List all output files that are in Allas using custom function
bucket_list <- list_all_objects_in_bucket(bucket = allas_opts$bucket, 
                                          region = allas_opts$opts$region, 
                                          prefix = bucket_list_prefix,
                                          only_keys = T)




# LOAD_FILES ---------------------------------------------------------------



idx <- 200
plot_file <- bucket_list[idx]

dt <- s3read_using(fread, object = plot_file, bucket = allas_opts$bucket, opts = allas_opts$opts)



# LOAD_INGROWTH_FILES -----------------------------------------------------------

# TEST PLOTS FOR INGROWTH = F RUNS

grobs_list <- list()

ingrowth <- F
idx <- 1

# Load from allas when ingrowth=T
if(ingrowth) {
  files_ingT <- grep("8003115_his", bucket_list, value = T)
  plot_file <- files_ingT[idx]
  print(paste0("Ingrowth is ", ingrowth))
  print(paste0("Loading from allas"))
  dt <- s3read_using(fread, object = plot_file, bucket = allas_opts$bucket, opts = allas_opts$opts)
} else {
  files_ingF <- list.files(file.path(output_base_path, "output_files"), full.names = T)
  plot_file <- files_ingF[idx]
  print(paste0("Ingrowth is ", ingrowth))
  print(paste0("Loading from filesystem"))
  dt <- readRDS(plot_file)
}



# PARAMS ------------------------------------------------------------------



country <- dt$Country[[1]]
clim_scen <- dt$Climate_scenario[[1]]
man_scen <- dt$Management_scenario[[1]]
plot_vars <- c("gpp", "gai", "vol")



# FILTER_TABLE ---------------------------------------------------------------



dt_plot_vars <- dt[Variable %in% plot_vars]



# PLOT --------------------------------------------------------------------



create_yearly_avgs_plot <- function(data, by_param, country, clim_scen, man_scen, ingrowth = T) {
  ggplot(data, aes(x = Year, y = AvgSumValue, color = as.factor(data[[by_param]]))) +
    geom_line(size = 0.5) +
    geom_point(size = 0.5) +
    facet_wrap(~Variable, scales = "free_y") +  # Separate plots with independent y-axes
    labs(
      title = paste(
        "Yearly Averages of Sums for Each Variable by", by_param, "\n",
        "Country:", country, "\n",
        "Climate scenario:", clim_scen, "\n",
        "Management scenario:", man_scen, "\n",
        "Ingrowth:", ingrowth
      ),
      x = "Year",
      y = "Average Sum Value",
      color = by_param
    ) +
    theme_minimal()
}

by_params <- c("Layer", "Species")
plots <- lapply(by_params, function(by_param) {
  dt_sums <- dt_plot_vars[, .(SumValue = sum(Value)), by = c("Year", "Variable", by_param)]
  dt_avg <- dt_sums[, .(AvgSumValue = mean(SumValue, na.rm = TRUE)), by = c("Year", "Variable", by_param)]
  create_yearly_avgs_plot(data = dt_avg, by_param = by_param, country = country,
                          clim_scen = clim_scen, man_scen = man_scen, ingrowth = ingrowth)
}) # Create all plots

grobs_list <- c(plots, grobs_list)



# RUN WHEN GROBS LIST IS COMPLETE
grid_plot <- grid.arrange(grobs = grobs_list)





# SAVE --------------------------------------------------------------------



plot_filename <- unlist(tstrsplit(basename(plot_file), split = "\\.", keep = 1))
plot_save_filename <- paste0(plot_filename,".pdf")
plot_path <- "data/acc/plots"
plot_save_path <- file.path(plot_path, plot_save_filename)

ggsave(
  filename = plot_save_path,  # File path
  plot = cowplot::ggdraw(grid_plot),  # Wrap the grid object with ggdraw from cowplot
  width = 12,   # Adjust width
  height = 8,   # Adjust height
  dpi = 300     # Resolution
)
















