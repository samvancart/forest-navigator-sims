source('scripts/settings.R')
source('./r/plots.R')

# Plot layer sums and means

# Run multiSiteLayers.R and then modout_sums_and_means.R to get tabXs.

print(paste0("Running plotsSumsMeans.R"))

# Load tabX trees
fileName <- (paste0(rdata_path, "tabX_sums_means_",VAR_layer_names[1],".rdata"))
load(fileName)
tabX_trees <- tabX

# Load tabX clusters
fileName <- (paste0(rdata_path, "tabX_sums_means_",VAR_layer_names[2],".rdata"))
load(fileName)
tabX_clusters <- tabX

# Combine data frames
tabX <- rbind(tabX_trees,tabX_clusters)

# Define nSites
nSites <- length(unique(tabX_clusters$site))
# nSites <- max(as.integer(levels(tabX_clusters$site)))

# Load multiOut for varNames
fileName <- (paste0(rdata_path, "multiOut_",VAR_layer_names[1],".rdata"))

# Set varNames
varNames <- as.vector(unlist(dimnames(multiOut)[3]))

# Define variables
varXs <- c(11:14,17,18,30,43)

# Add Lc to names
varNames[55] = "Lc"

# Add lc to varXs
varXs <- append(varXs,55)

tabX$site <- as.factor(tabX$site)
tabX$layerType <- as.factor(tabX$layerType)

plotsVars_trees <- list(list())
plotsVars_clusters <- list(list())
plotsVars <- list(list())

print("Creating plots...")
# Plot
# CHECK VAR_data_from IN SETTINGS
for (siteX in 1:nSites) {
  for(variableX in varNames[varXs]){
    plotsVars <- get_sums_means_plotsVars(plotsVars,variableX,siteX,tabX, VAR_data_from)

  }
  # CHECK VAR_data_from IN SETTINGS
  plot_path <- get_by_site_plot_path("sums_means",siteX, VAR_data_from)
  pdf(plot_path)
  
  for(variableX in varNames[varXs]){
    print(plotsVars[[variableX]])
  }
  dev.off()
}

folder_path <- get_folder_path_from_plot_path(plot_path)
print(paste0("Plots saved to ", folder_path))


# # print plots
# for(variableX in varNames[varXs]){
#   print(plotsVars[[variableX]])
# }

















