

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



filtered_bucket_list <- grep(man_name, bucket_list, value = T)




dt <- s3read_using(fread, object = bucket_list[20], bucket = allas_opts$bucket, opts = allas_opts$opts)

plot_vars <- c("gpp", "gai", "vol")

dt_plot_vars <- dt[Variable %in% plot_vars]

lapply(plot_vars, function(var) {
  hist(x = dt_plot_vars[Variable==var]$Value, main = var)
})



dt_sums <- dt_plot_vars[, .(SumValue = sum(Value)), by = .(Year, Variable, Layer)]
dt_avg <- dt_sums[, .(AvgSumValue = mean(SumValue, na.rm = TRUE)), by = .(Year, Layer, Variable)]

chosen_variable <- "gpp"
dt_var <- dt_avg[Variable==chosen_variable]

ggplot(dt_var, aes(x = Year, y = AvgSumValue, color = Variable)) +
  geom_line(size = 0.5) + # Adjust the line thickness here
  facet_wrap(~Layer) +
  labs(
    title = "Yearly Averages of Sums by Variable and Layer",
    x = "Year",
    y = "Average Sum Value"
  ) +
  theme_minimal()

ggplot(dt_avg, aes(x = Year, y = AvgSumValue, color = as.factor(Layer))) +
  geom_line(size = 0.5) +
  geom_point(size = 0.5) +
  facet_wrap(~Variable, scales = "free_y") +  # Separate plots with independent y-axes
  labs(
    title = "Yearly Averages of Sums for Each Variable by Layer",
    x = "Year",
    y = "Average Sum Value",
    color = "Layer"
  ) +
  theme_minimal()













