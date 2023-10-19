source('scripts/settings.R')
source('./r/plots.R')


plot_residuals <- function(tabX, sub_folder, varNames, varXs, shape="species", point_col="layer", box_col="species", fill = "layer") {
  point_plots <- list(list())
  boxplots <- list(list())
  print(tabX)
  print(sub_folder)
  
  # Plot
  for (siteX in 1:nSites) {
    for(variableX in varNames[varXs]){
      point_plots <- get_residuals_pointplots(point_plots,variableX,siteX,tabX, data_from, shape=shape, col=point_col)
      boxplots <- get_residuals_boxplots(boxplots,variableX,siteX,tabX, data_from, col=box_col, fill=fill)

    }
    plot_path <- get_by_site_plot_path("side_by_side",siteX, data_from, sub_folder = sub_folder)
    pdf(plot_path, width=14, height=7)
    for(variableX in varNames[varXs]){
      print(grid.arrange(point_plots[[variableX]],boxplots[[variableX]],ncol=2))
    }
    dev.off()
  }
}




print(paste0("Running residuals.R"))

# Load tabX trees
fileName <- (paste0(rdata_path, "tabX_layerAggr_", layerNames[1],".rdata"))
load(fileName)
tabX_trees <- tabX

# Load tabX clusters
fileName <- (paste0(rdata_path, "tabX_layerAggr_", layerNames[2],".rdata"))
load(fileName)
tabX_clusters <- tabX

setnames(tabX_clusters,"value","cluster")
setnames(tabX_trees,"value","tree")
tabX <- merge(tabX_clusters,tabX_trees)
tabX[,residuals:=cluster-tree]

tabX$layer <- as.factor(tabX$layer)
tabX$year <- as.factor(tabX$year)

# Load multiOut for varNames
fileName <- (paste0(rdata_path, "multiOut_",layerNames[1],".rdata"))
load(fileName)

# Set nSites
nSites <- max(as.integer(tabX$site))

# Set varNames
varNames <- as.vector(unlist(dimnames(multiOut)[3]))

# Define variables
varXs <- c(11:14,17,18,30,43)

# Add Lc to names
varNames[55] = "Lc"

# Add lc to varXs
varXs <- append(varXs,55)

plot_residuals(tabX = tabX, sub_folder =  paste0("/residuals/layer/"), varNames = varNames, varXs = varXs)


tabX_trees_aggrSpecies <- tabX_trees[variable %in% c("BA","V","grossGrowth"),sum(tree),by=.(site,groupID,speciesID,year,species,variable)]
tabX_clusters_aggrSpecies <- tabX_clusters[variable %in% c("BA","V","grossGrowth"),sum(cluster),by=.(site,groupID,speciesID,year,species,variable)]
setnames(tabX_trees_aggrSpecies,"V1","tree")
setnames(tabX_clusters_aggrSpecies,"V1","cluster")
tabX <- merge(tabX_clusters_aggrSpecies,tabX_trees_aggrSpecies)
tabX[,residuals:=cluster-tree]

varXs <- c(13,30,43)

plot_residuals(tabX = tabX, sub_folder =  paste0("/residuals/species/"), varNames = varNames, varXs = varXs, shape = NULL, point_col="species", box_col = "species", fill=NULL)


# !rename and calculate residuals and make plots similar to below

tabX_trees_aggrSite <- tabX_trees[variable %in% c("BA","V","grossGrowth"),sum(tree),by=.(site,groupID,year,variable)]
tabX_clusters_aggrSite <- tabX_clusters[variable %in% c("BA","V","grossGrowth"),sum(cluster),by=.(site,groupID,year,variable)]
setnames(tabX_trees_aggrSite,"V1","tree")
setnames(tabX_clusters_aggrSite,"V1","cluster")
tabX <- merge(tabX_clusters_aggrSite,tabX_trees_aggrSite)
tabX[,residuals:=cluster-tree]

plot_residuals(tabX = tabX, sub_folder =  paste0("/residuals/site/"), varNames = varNames, varXs = varXs, shape = NULL, point_col=NULL, box_col = NULL, fill=NULL)

# !rename and calculate residuals and make plots similar to below





# 
# ggplot(data = tabX[site==21 & variable=="grossGrowth"], aes_string(x = "year",y = "residuals",shape="species",col="layer")) +
#   geom_point() #+ ylim(-5,5) + geom_hline(yintercept=c(-1,1))
# 
# ggplot(data = tabX[site==21 & variable=="grossGrowth"], aes(y = residuals,col=species,fill=layer)) +
#   geom_boxplot() #+ ylim(-5,5) + geom_hline(yintercept=c(-1,1))
