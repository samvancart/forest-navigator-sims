source('scripts/settings.R')
source('./r/plots.R')
source('./r/utils.R')


plot_residuals <- function(tabX, sub_folder, varNames, varXs, shape="species", point_col="layer", box_col="species", fill = "layer") {
  point_plots <- list(list())
  boxplots <- list(list())
  
  # Set nSites
  nSites <- max(as.integer(tabX$site))
  
  print(paste0("Creating ", sub_folder, " plots..."))
  
  # Plot
  for (siteX in 1:nSites) {
    for(variableX in varNames[varXs]){
      point_plots <- get_residuals_pointplots(point_plots,variableX,siteX,tabX, data_from, shape=shape, col=point_col)
      boxplots <- get_residuals_boxplots(boxplots,variableX,siteX,tabX, data_from, col=box_col, fill=fill)

    }
    plot_path <- get_by_site_plot_path("side_by_side",siteX, data_from, sub_folder = sub_folder)
    pdf(plot_path, width=14, height=7)
    for(variableX in varNames[varXs]){
      grid.arrange(point_plots[[variableX]],boxplots[[variableX]],ncol=2)
    }
    dev.off()
  }
  
  folder_path <- get_folder_path_from_plot_path(plot_path)
  print(paste0("Plots saved to ", folder_path))
}


plot_combined_residuals <- function(tabX, plot_path, formula=as.formula(paste("~", "species")), col="species", fill="species") {
  
  tabX$year <- as.factor(tabX$year)
  
  path <- paste0(plot_path,"combined.pdf")
  pdf(file=path)
  
  print("Plotting combined...")
  
  for(varX in unique(tabX$variable)){
    p1 <- ggplot(data = tabX[variable==varX], aes_string(x = "year", y = "residuals", col=col)) +
      geom_boxplot()
    
    p2 <- ggplot(data = tabX[variable==varX], aes_string(x = "residuals", fill=fill)) +
      geom_histogram(bins = 30) + facet_wrap(formula)
    plotX <- ggarrange(p1,p2,nrow=2)
    
    plotX <-annotate_figure(plotX, top = text_grob(varX, face = "bold", size = 14))
    print(plotX)
  }
  dev.off()
  folder_path <- get_folder_path_from_plot_path(plot_path)
  print(paste0("Plots saved to ", folder_path))
}


prepare_tabX <- function(trees, clusters, old_val) {
  setnames(clusters,old_val,"cluster")
  setnames(trees,old_val,"tree")
  tabX <- merge(clusters,trees)
  tabX <- tabX[,residuals:=cluster-tree]
  
  return(tabX)
}


print(paste0("Running plotsResiduals.R"))

# Load tabX trees
fileName <- (paste0(rdata_path, "tabX_layerAggr_", layerNames[1],".rdata"))
load(fileName)
tabX_trees <- tabX

# Load tabX clusters
fileName <- (paste0(rdata_path, "tabX_layerAggr_", layerNames[2],".rdata"))
load(fileName)
tabX_clusters <- tabX

# Prepare tabX
tabX <- prepare_tabX(tabX_trees,tabX_clusters,old_val = "value")

# To factor
tabX <- to_factor(tabX, c("layer"))

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

# Plot
plot_residuals(tabX = tabX, sub_folder =  paste0("/residuals/layer/"), varNames = varNames, varXs = varXs)





### Aggregate by species

# Define vars
varXs <- c(13,30,43)

tabX_trees_aggrSpecies <- tabX_trees[variable %in% c("BA","V","grossGrowth"),sum(tree),by=.(site,groupID,speciesID,year,species,variable)]
tabX_clusters_aggrSpecies <- tabX_clusters[variable %in% c("BA","V","grossGrowth"),sum(cluster),by=.(site,groupID,speciesID,year,species,variable)]

# Prepare tabX
tabX <- prepare_tabX(tabX_trees_aggrSpecies,tabX_clusters_aggrSpecies,old_val = "V1")

# Plot species
plot_residuals(tabX = tabX, sub_folder =  paste0("/residuals/species/"), varNames = varNames, varXs = varXs, shape = NULL, point_col="species", box_col = "species", fill=NULL)

# Plot combined
species_plot_path <-paste0("data/plots/by_site/side_by_side/residuals/species/", data_from, "/combined/")
plot_combined_residuals(tabX = tabX, plot_path = species_plot_path, col = "species", fill = "species")





### Aggregate by site

tabX_trees_aggrSite <- tabX_trees[variable %in% c("BA","V","grossGrowth"),sum(tree),by=.(site,groupID,year,variable)]
tabX_clusters_aggrSite <- tabX_clusters[variable %in% c("BA","V","grossGrowth"),sum(cluster),by=.(site,groupID,year,variable)]

# Prepare tabX
tabX <- prepare_tabX(tabX_trees_aggrSite,tabX_clusters_aggrSite,old_val = "V1")

# Plot site
plot_residuals(tabX = tabX, sub_folder =  paste0("/residuals/site/"), varNames = varNames, varXs = varXs, shape = NULL, point_col=NULL, box_col = NULL, fill=NULL)

# Plot combined
site_plot_path <-paste0("data/plots/by_site/side_by_side/residuals/site/", data_from, "/combined/")
plot_combined_residuals(tabX = tabX, plot_path = site_plot_path, formula = NULL, col = NULL, fill = NULL)









# ggplot(data = tabX[site==4 & variable=="grossGrowth"], aes_string(x = "year",y = "residuals",shape="species",col="layer")) +
#   geom_point() #+ ylim(-5,5) + geom_hline(yintercept=c(-1,1))
# 
# ggplot(data = tabX[site==21 & variable=="grossGrowth"], aes(y = residuals,col=species,fill=layer)) +
#   geom_boxplot() #+ ylim(-5,5) + geom_hline(yintercept=c(-1,1))
