source('scripts/settings.R')
source('./r/plots.R')


# Plot tree and cluster layers side by side

# Run multiSiteLayers and then plot_tables.R to get tabXs.

# Load tabX trees
fileName <- (paste0(rdata_path, "tabX_",layerNames[1],".rdata"))
load(fileName)
tabX_trees <- tabX

# Load tabX clusters
fileName <- (paste0(rdata_path, "tabX_",layerNames[2],".rdata"))
load(fileName)
tabX_clusters <- tabX

# Define nSites
nSites <- max(as.integer(levels(tabX_clusters$site)))

# Load multiOut for varNames
fileName <- (paste0(rdata_path, "multiOut_",layerNames[1],".rdata"))
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


# Plot
# CHECK VARIABLE 'data_from' IN settings.R
for (siteX in 1:nSites) {
  for(variableX in varNames[varXs]){
    plotsVars_trees <- get_plotsVars(plotsVars_trees,variableX,siteX,tabX_trees,1, data_from)
    plotsVars_clusters <- get_plotsVars(plotsVars_clusters,variableX,siteX,tabX_clusters,2, data_from)
    
  }
  # CHECK VARIABLE 'data_from' IN settings.R
  plot_path <- get_by_site_plot_path("side_by_side",siteX, data_from)
  pdf(plot_path, width=14, height=7)
  for(variableX in varNames[varXs]){
    print(grid.arrange(plotsVars_trees[[variableX]],plotsVars_clusters[[variableX]],ncol=2))
  }
  dev.off()
}






# # print plots
# for(variableX in varNames[varXs]){
#   print(plotsVars_clusters[[variableX]])
# }

# # Plot side by side
# for(variableX in varNames[varXs]){
#   print(grid.arrange(plotsVars_trees[[variableX]],plotsVars_clusters[[variableX]],ncol=2))
# }