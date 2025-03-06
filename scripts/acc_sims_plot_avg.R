# Plot yearly averages for sums of variables by layer and species.

# sourceFiles -------------------------------------------------------------

source('scripts/settings.R')
source(config$PATH_acc_sims_prepare_init_settings)


# getFilesToPlot -----------------------------------------------------------


bucket_list_prefix <- file.path("output", simulation_site, "output_files")

# List all output files that are in Allas using custom function
bucket_list <- list_all_objects_in_bucket(bucket = allas_opts$bucket, 
                                          region = allas_opts$opts$region, 
                                          prefix = bucket_list_prefix,
                                          only_keys = T)




# loadFiles ---------------------------------------------------------------

idx <- 200
plot_file <- bucket_list[idx]

dt <- s3read_using(fread, object = plot_file, bucket = allas_opts$bucket, opts = allas_opts$opts)



# params ------------------------------------------------------------------



country <- dt$Country[[1]]
clim_scen <- dt$Climate_scenario[[1]]
man_scen <- dt$Management_scenario[[1]]
plot_vars <- c("gpp", "gai", "vol")



# filterTable ---------------------------------------------------------------



dt_plot_vars <- dt[Variable %in% plot_vars]



# plot --------------------------------------------------------------------



create_yearly_avgs_plot <- function(data, by_param, country, clim_scen, man_scen) {
  ggplot(data, aes(x = Year, y = AvgSumValue, color = as.factor(data[[by_param]]))) +
    geom_line(size = 0.5) +
    geom_point(size = 0.5) +
    facet_wrap(~Variable, scales = "free_y") +  # Separate plots with independent y-axes
    labs(
      title = paste(
        "Yearly Averages of Sums for Each Variable by", by_param, "\n",
        "Country:", country, "\n",
        "Climate scenario:", clim_scen, "\n",
        "Management scenario:", man_scen
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
                          clim_scen = clim_scen, man_scen = man_scen)
}) # Create all plots

grid_plot <- grid.arrange(grobs = plots)




# save --------------------------------------------------------------------



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
















