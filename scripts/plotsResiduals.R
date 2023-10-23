source('scripts/settings.R')
source('./r/plots.R')
source('./r/utils.R')
source('./r/residuals.R')


# Calculates and plots residuals on layer level, species level and site level

# Before running: Run layerAggr.R to get aggregated tabXs.


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

tabX_trees_aggrSpecies <- tabX_trees[variable %in% c("BA","V","grossGrowth"),
                                     sum(tree),by=.(site,groupID,speciesID,year,species,variable)]
tabX_clusters_aggrSpecies <- tabX_clusters[variable %in% c("BA","V","grossGrowth"),
                                           sum(cluster),by=.(site,groupID,speciesID,year,species,variable)]

# Prepare tabX
tabX <- prepare_tabX(tabX_trees_aggrSpecies,tabX_clusters_aggrSpecies,old_val = "V1")

# Plot species
plot_residuals(tabX = tabX, sub_folder =  paste0("/residuals/species/"), 
               varNames = varNames, varXs = varXs, shape = NULL, point_col="species", box_col = "species", fill=NULL)

# Plot combined
species_plot_path <-paste0("data/plots/by_site/side_by_side/residuals/species/", data_from, "/combined/")
plot_combined_residuals(tabX = tabX, plot_path = species_plot_path, col = "species", fill = "species")





### Aggregate by site

tabX_trees_aggrSite <- tabX_trees[variable %in% c("BA","V","grossGrowth"),sum(tree),by=.(site,groupID,year,variable)]
tabX_clusters_aggrSite <- tabX_clusters[variable %in% c("BA","V","grossGrowth"),sum(cluster),by=.(site,groupID,year,variable)]

# Prepare tabX
tabX <- prepare_tabX(tabX_trees_aggrSite,tabX_clusters_aggrSite,old_val = "V1")

# Plot site
plot_residuals(tabX = tabX, sub_folder =  paste0("/residuals/site/"), 
               varNames = varNames, varXs = varXs, shape = NULL, point_col=NULL, box_col = NULL, fill=NULL)

# Plot combined
site_plot_path <-paste0("data/plots/by_site/side_by_side/residuals/site/", data_from, "/combined/")
plot_combined_residuals(tabX = tabX, plot_path = site_plot_path, formula = NULL, col = NULL, fill = NULL)























