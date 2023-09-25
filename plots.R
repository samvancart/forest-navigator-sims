source('settings.R')

# Functions

get_plotsVars <- function(plotsVars,variableX,siteX,tabX,layerNumber) {
  plotsVars[[variableX]] <- ggplot(data=tabX[variable==variableX & site==siteX],  aes(x=year,y=value,col=species, group=layer)) +
    geom_line() +
    ggtitle(paste0("Site ",siteX," ",layerNames[layerNumber]),variableX)
  
  return(plotsVars)
}



# Load tabX trees
fileName <- (paste0("tabX_",layerNames[1],".rdata"))
load(fileName)
tabX_trees <- tabX

# Load tabX clusters
fileName <- (paste0("tabX_",layerNames[2],".rdata"))
load(fileName)
tabX_clusters <- tabX

# Define nSites
nSites <- max(as.integer(levels(tabX_clusters$site)))

# Load multiOut for varNames
fileName <- (paste0("multiOut_",layerNames[1],".rdata"))
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
for (siteX in 1:nSites) {
  for(variableX in varNames[varXs]){
    plotsVars_trees <- get_plotsVars(plotsVars_trees,variableX,siteX,tabX_trees,1)
    plotsVars_clusters <- get_plotsVars(plotsVars_clusters,variableX,siteX,tabX_clusters,2)
    
  }
  pdf_out <- paste0(layerNames[2],"_as_layers/site_",siteX)
  side_by_side <- paste0("side_by_side/site_", siteX)
  plot_path <- paste0("C:/Users/samu/Documents/yucatrote/r/forest_navigator23_r/data/plots/by_site/", side_by_side, ".pdf")
  pdf(plot_path, width=14, height=7)
  for(variableX in varNames[varXs]){
    # print(plotsVars_clusters[[variableX]])
    print(grid.arrange(plotsVars_trees[[variableX]],plotsVars_clusters[[variableX]],ncol=2))
  }
  dev.off()
}



# # plots to pdf
# pdf_out <- paste0(species,"_plots_", estimatedName)
# pdf_out
# plot_path <- paste0("C:/Users/samu/Documents/yucatrote/r/forest_navigator23_r/data/plots/", pdf_out, ".pdf")
# pdf(plot_path)
# for(variableX in varNames[varXs]){
#   print(plotsVars[[variableX]])
# }
# dev.off()

# # print plots
# for(variableX in varNames[varXs]){
#   print(plotsVars_clusters[[variableX]])
# }

# # Plot side by side
# for(variableX in varNames[varXs]){
#   print(grid.arrange(plotsVars_trees[[variableX]],plotsVars_clusters[[variableX]],ncol=2))
# }

