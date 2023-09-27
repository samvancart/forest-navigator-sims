# source('settings.R')
# 
# # Functions
# 
# get_plotsVars <- function(plotsVars,variableX,siteX,tabX,layerNumber) {
#   plotsVars[[variableX]] <- ggplot(data=tabX[variable==variableX & site==siteX],  aes(x=year,y=value,col=species, group=layer)) +
#     geom_line() +
#     ggtitle(paste0("Site ",siteX," ",layerNames[layerNumber]),variableX)
# 
#   return(plotsVars)
# }
# 
# get_sums_means_plotsVars <- function(plotsVars,variableX,siteX,tabX) {
#   plotsVars[[variableX]] <- ggplot() +
#     geom_line(data=tabX[variable==variableX & site==siteX],  aes(x=year,y=value,group=LayerType,col=LayerName)) +
#     labs(col = "Layer Name") +
#     ggtitle(paste0("Site ",siteX," "),variableX)
#   
#   return(plotsVars)
# }
# 
# 
# 
# # Load tabX trees
# fileName <- (paste0(rdata_path, "tabX_sums_means_",layerNames[1],".rdata"))
# load(fileName)
# tabX_trees <- tabX
# 
# # Load tabX clusters
# fileName <- (paste0(rdata_path, "tabX_sums_means_",layerNames[2],".rdata"))
# load(fileName)
# tabX_clusters <- tabX
# 
# # Combine data frames
# tabX <- rbind(tabX_trees,tabX_clusters)
# 
# # Define nSites
# nSites <- length(unique(tabX_clusters$site))
# # nSites <- max(as.integer(levels(tabX_clusters$site)))
# 
# # Load multiOut for varNames
# fileName <- (paste0(rdata_path, "multiOut_",layerNames[1],".rdata"))
# 
# # Set varNames
# varNames <- as.vector(unlist(dimnames(multiOut)[3]))
# 
# # Define variables
# varXs <- c(11:14,17,18,30,43)
# 
# # Add Lc to names
# varNames[55] = "Lc"
# 
# # Add lc to varXs
# varXs <- append(varXs,55)
# 
# tabX$site <- as.factor(tabX$site)
# tabX$layerType <- as.factor(tabX$layerType)
# 
# plotsVars_trees <- list(list())
# plotsVars_clusters <- list(list())
# plotsVars <- list(list())
# 
# # Plot
# for (siteX in 1:nSites) {
#   for(variableX in varNames[varXs]){
#     # plotsVars_trees <- get_plotsVars(plotsVars_trees,variableX,siteX,tabX_trees,1)
#     # plotsVars_clusters <- get_plotsVars(plotsVars_clusters,variableX,siteX,tabX_clusters,2)
#     plotsVars <- get_sums_means_plotsVars(plotsVars,variableX,siteX,tabX)
# 
#   }
#   
#   # pdf_out <- paste0(layerNames[2],"_as_layers/site_",siteX)
#   # side_by_side <- paste0("side_by_side/site_", siteX)
#   # plot_path <- paste0("C:/Users/samu/Documents/yucatrote/r/forest_navigator23_r/data/plots/by_site/", side_by_side, ".pdf")
#   # pdf(plot_path, width=14, height=7)
#   # for(variableX in varNames[varXs]){
#     # print(plotsVars_clusters[[variableX]])
#     # print(grid.arrange(plotsVars_trees[[variableX]],plotsVars_clusters[[variableX]],ncol=2))
#   # }
#   # dev.off()
# }
# 
# 
# # # plots to pdf
# # pdf_out <- paste0(species,"_plots_", estimatedName)
# # pdf_out
# # plot_path <- paste0("C:/Users/samu/Documents/yucatrote/r/forest_navigator23_r/data/plots/", pdf_out, ".pdf")
# # pdf(plot_path)
# # for(variableX in varNames[varXs]){
# #   print(plotsVars[[variableX]])
# # }
# # dev.off()
# 
# # # print plots
# # for(variableX in varNames[varXs]){
# #   print(plotsVars[[variableX]])
# # }
# 
# # Plot side by side
# # for(variableX in varNames[varXs]){
# #   # print(grid.arrange(plotsVars_trees[[variableX]],plotsVars_clusters[[variableX]],ncol=2))
# #   print(grid.arrange(plotsVars[[variableX]]))
# # }
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 



source('settings.R')


# Functions

# Melt Lc
melt_lc <- function(lc,runID,siteType){
  melt_lc <- data.table(melt(lc),SiteType=siteType)
  colnames(melt_lc) <- c("site","year","value","SiteType")
  melt_lc$variable <- "Lc"
  melt_lc$runID <- runID
  return(melt_lc)
}


get_lc_from_multiOut <- function(multiOut) {
  lc <- multiOut[,,11,,1] - multiOut[,,14,,1]
  return(lc)
}

build_tabX <- function(multiOut, multiOut_st1, multiOut_st5, varXs) {
  
  
  # Length of crown: H-Hc
  lc_stN <-  get_lc_from_multiOut(multiOut)
  lc_st1 <-  get_lc_from_multiOut(multiOut_st1)
  lc_st5 <-  get_lc_from_multiOut(multiOut_st5)
  
  
  # Create tabX
  tabX <- data.table(melt(multiOut_st1[,,varXs,1,1]),SiteType=1); tabX$runID ="st1"
  melt_lc_st1 <- melt_lc(lc_st1,"st1",1)
  tabX <- rbind(tabX,melt_lc_st1)
  tabX <- rbind(tabX,data.table(melt(multiOut_st5[,,varXs,1,1]),SiteType=5,runID ="st5"))
  melt_lc_st5 <- melt_lc(lc_st1,"st5",5)
  tabX <- rbind(tabX,melt_lc_st5)
  
  
  tabXst<- data.table(melt(multiOut[,,3,1,1]))
  setnames(tabXst,c("site","year","SiteType"))
  tabXvars<- data.table(melt(multiOut[,,varXs,1,1]))
  melt_lc_stN <- melt_lc(lc_stN,"N_based",tabXst$SiteType)
  tabXestimated <- merge(tabXst,tabXvars)
  tabXestimated$runID <- "N_based"
  tabX <- rbind(tabX,tabXestimated)
  tabX <- rbind(tabX,melt_lc_stN)
  

  # As factor
  tabX$site <- as.factor(tabX$site)
  tabX$SiteType <- as.factor(tabX$SiteType)
  tabX$runID <- as.factor(tabX$runID)
  
  return(tabX)
}

get_shadow_plotsVars <- function(plotsVars, variableX, tabX) {
  plotsVars[[variableX]] <- ggplot() +
    stat_summary(data=tabX[variable==variableX & runID %in% c("st1","st5")],aes(x=year,y=value),geom = "ribbon", fun.min = min, fun.max = max, alpha = 0.3, colour=NA) +
    geom_line(data=tabX[variable==variableX & runID %in% "N_based"],  aes(x=year,y=value,group=site,col=SiteType)) +
    ggtitle(species, variableX) +
    labs(caption = paste0(estimatedName," ", list(round(estimated,2))))
  
  return (plotsVars)
}


fileName <- paste0(rdata_path, "multiOut_spID",speciesID,".rdata")
load(fileName)
multi_after <- multiOut
multi_after_st1 <- multiOut_st1
multi_after_st5 <- multiOut_st5

species <- speciesNames[speciesID]
estimatedName <- estimatedNames[estimatedID]

# define variables
varXs <- c(11:14,17,18,30,43)

tabX_after <- build_tabX(multi_after, multi_after_st1, multi_after_st5, varXs)

# Lc to varXs
varXs <- append(varXs,55)

plotsVars <- list()


# Plot with shadow
for(variableX in varNames[varXs]){
  plotsVars <- get_shadow_plotsVars(plotsVars, variableX, tabX_after)
}

# # display all plots in grid
# grid.arrange(grobs = plotsVars)
# dev.off

# # plots to pdf
# pdf_out <- paste0(species,"_plots_", estimatedName)
# pdf_out
# plot_path <- paste0("C:/Users/samu/Documents/yucatrote/r/forest_navigator23_r/data/plots/", pdf_out, ".pdf")
# pdf(plot_path)
# for(variableX in varNames[varXs]){
#   print(plotsVars[[variableX]])
# }
# dev.off()

# print plots
for(variableX in varNames[varXs]){
  print(plotsVars[[variableX]])
}
