source('scripts/settings.R')
source('./r/plots.R')


# Plot tree and cluster layers side by side

# Run multiSiteLayers and then either
#   1.  plot_tables.R -> VAR_tabX_id = 1
# or
#   2.  layerAggr.R -> VAR_tabX_id = 2

# Run with desired params from run.R 

print(paste0("Running plotsLayers.R for ", VAR_tabX_names[VAR_tabX_id]))

# Load tabX trees
fileName <- (paste0(PATH_rdata, "tabX_", VAR_tabX_names[VAR_tabX_id], "_", VAR_layer_names[1],".rdata"))
load(fileName)
tabX_trees <- tabX

# Load tabX clusters
fileName <- (paste0(PATH_rdata, "tabX_", VAR_tabX_names[VAR_tabX_id], "_",VAR_layer_names[2],".rdata"))
load(fileName)
tabX_clusters <- tabX

# Define nSites
nSites <- max(as.integer(levels(tabX_clusters$site)))

# Load multiOut for varNames
fileName <- (paste0(PATH_rdata, "multiOut_",VAR_layer_names[1],".rdata"))
load(fileName)
# Set varNames
varNames <- as.vector(unlist(dimnames(multiOut)[3]))

# Define variables
varXs <- c(11:14,17,18,30,43)

# Add Lc to names
varNames[55] = "Lc"

# Add lc to varXs
varXs <- append(varXs,55)

plotsVars_trees <- list(list())
plotsVars_clusters <- list(list())

# Subfolder for plots
sub_folder <- paste0("/",VAR_tabX_names[VAR_tabX_id],"/")

print("Creating plots...")

# Plot
# CHECK VARIABLE 'VAR_data_from' IN settings.R
for (siteX in 1:nSites) {
  for(variableX in varNames[varXs]){
    plotsVars_trees <- get_plotsVars(plotsVars_trees,variableX,siteX,tabX_trees,1, VAR_data_from)
    plotsVars_clusters <- get_plotsVars(plotsVars_clusters,variableX,siteX,tabX_clusters,2, VAR_data_from)
    
  }
  # CHECK VARIABLE 'VAR_data_from' IN settings.R
  plot_path <- get_by_site_plot_path("side_by_side",siteX, VAR_data_from, sub_folder = sub_folder)
  pdf(plot_path, width=14, height=7)
  for(variableX in varNames[varXs]){
    grid.arrange(plotsVars_trees[[variableX]],plotsVars_clusters[[variableX]],ncol=2)
  }
  dev.off()
}


folder_path <- get_folder_path_from_plot_path(plot_path)
print(paste0("Plots saved to ", folder_path))



# # print plots
# for(variableX in varNames[varXs]){
#   print(plotsVars_clusters[[variableX]])
# }

# # Plot side by side
# for(variableX in varNames[varXs]){
#   print(grid.arrange(plotsVars_trees[[variableX]],plotsVars_clusters[[variableX]],ncol=2))
# }
