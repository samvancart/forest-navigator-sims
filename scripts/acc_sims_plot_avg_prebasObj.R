
# SOURCE_FILES -------------------------------------------------------------

source('scripts/settings.R')
source(config$PATH_acc_sims_prepare_init_settings)


# LOAD_FILES ---------------------------------------------------------------

test_files <- list.files(output_base_path, recursive = T, full.names = T)
test_filepaths <- grep("testRun", test_files, value = T)
print(test_filepaths)

plgid <- unlist(tstrsplit(basename(test_filepaths[1]), split = "_", keep = 3))


# GET_PLOT-DT_FROM_MULTIOUT ---------------------------------------------------


process_annual_avg_by_all_and_siteType_from_multiOut <- function(multiOut, vars, varNames) {
  all_results <- list()  # To store results for all vars
  
  for (var in vars) {  # Loop over vars

    # Calculate stand_tot and stand_mean for the current var
    stand_tot <- apply(multiOut[,,var,,1], 1:2, sum)
    stand_mean <- apply(stand_tot, 2, mean)
    
    # Create stand_mean_dt for the current var
    stand_mean_dt <- rbindlist(lapply(seq(stand_mean), function(i) {
      data.table(year = i, site_type = "all", mean_value = stand_mean[i], var = varNames[var])
    }))
    
    # Extract siteTypes matrix for the current var
    siteTypes <- multiOut[,,3,1,1]
    
    # Initialize an empty list for results
    results <- list()
    
    # Loop through each year (column) of the stand_tot matrix
    for (year in 1:ncol(stand_tot)) {
      # Get the unique site types for the current year
      unique_site_types <- unique(siteTypes[, year])
      
      # Calculate mean values for each site type
      year_results <- sapply(unique_site_types, function(site_type) {
        mean(stand_tot[siteTypes[, year] == site_type, year], na.rm = TRUE)
      })
      
      # Add results to the list and name it by site type
      names(year_results) <- unique_site_types
      results[[paste0("Year_", year)]] <- year_results
    }
    
    # Convert the results list into a data.table
    results_dt <- rbindlist(
      lapply(seq_along(results), function(year_idx) {
        data.table(
          site_type = as.integer(names(results[[year_idx]])),
          year = year_idx,
          mean_value = unname(results[[year_idx]]),
          var = varNames[var]
        )
      })
    )
    
    # Combine results_dt and stand_mean_dt and store in all_results
    all_results[[var]] <- rbind(results_dt, stand_mean_dt)
  }
  
  # Combine all var results into a single data.table
  final_results_dt <- rbindlist(all_results)
  
  # Sort the final results by year and site_type
  setorder(final_results_dt, year, site_type)
  
  return(final_results_dt)
}

# Define a function to process each file
process_test_filepath <- function(filepath) {
  # Extract the filename and split to get the required parts
  filename <- basename(filepath)
  parts <- strsplit(filename, "_")[[1]]
  clim_scen <- parts[length(parts) - 2]  # Third-to-last part
  man_scen <- parts[length(parts) - 1]  # Second-to-last part
  ingrowth <- grepl("T", parts[length(parts)])  # True if the last part contains "T", else False
  
  # Read the file and extract the `multiOut` object
  data <- readRDS(filepath)
  multiOut <- data$multiOut
  
  # Call the processing function
  results_dt <- process_annual_avg_by_all_and_siteType_from_multiOut(multiOut, vars, varNames)
  
  # Add the extracted parts as new columns
  results_dt[, clim_scen := clim_scen]
  results_dt[, man_scen := man_scen]
  results_dt[, ingrowth := ingrowth]
  
  return(results_dt)
}




vars <- c(30, 43, 44)

# Use lapply to process all files and combine the results
final_results_dt <- rbindlist(lapply(test_filepaths, process_test_filepath))

# Print the final results
print(final_results_dt)



# PLOT --------------------------------------------------------------------



# Function to create grids of plots for each combination of man_scen and ingrowth
create_grids <- function(data) {
  # Get unique combinations of man_scen and ingrowth
  combinations <- unique(data[, .(man_scen, ingrowth)])
  
  # Create a list to store grids for each combination
  grids <- lapply(1:nrow(combinations), function(idx) {
    # Extract the current combination
    man_scen_value <- combinations[idx, man_scen]
    ingrowth_value <- combinations[idx, ingrowth]
    
    # Filter data for the current combination
    subset_data <- data[man_scen == man_scen_value & ingrowth == ingrowth_value]
    
    # Create plots for all vars for the current combination
    plots <- lapply(unique(subset_data$var), function(current_var) {
      # Filter data for the current var
      var_data <- subset_data[var == current_var]
      
      # Create a ggplot object
      ggplot(var_data, aes(x = year, y = mean_value, color = as.factor(site_type), group = site_type)) +
        geom_line(size = 0.5) +  # Add line graph
        geom_point(size = 0.2) + # Add points for clarity
        labs(
          title = paste("Variable:", current_var),
          x = "Year",
          y = "Mean Value",
          color = "Site Type"
        ) +
        theme_minimal() +  # Use a clean theme
        theme(
          plot.title = element_text(size = 10, face = "bold"),
          axis.text = element_text(size = 8),
          axis.title = element_text(size = 9)
        )
    })
    
    # Arrange plots into a grid
    grid <- grid.arrange(grobs = plots, ncol = 2, top = paste("man_scen:", man_scen_value, "| ingrowth:", ingrowth_value))
    return(grid)
  })
  
  return(grids)
}

# Call the function on your final_results_dt
grids <- create_grids(final_results_dt)

# To display the grids:
# Example: print the first grid
grid_plot <- grid.arrange(grobs=grids)



# SAVE --------------------------------------------------------------------




plot_filename <- paste0("avg-all-and-by-siteType_", plgid)
plot_save_filename <- paste0(plot_filename,".pdf")
plot_path <- "data/acc/plots"
plot_save_path <- file.path(plot_path, plot_save_filename)

ggsave(
  filename = plot_save_path,  # File path
  plot = cowplot::ggdraw(grid_plot),  # Wrap the grid object with ggdraw from cowplot
  width = 16,   # Adjust width
  height = 12,   # Adjust height
  dpi = 300     # Resolution
)










