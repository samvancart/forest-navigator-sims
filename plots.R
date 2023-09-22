

source('settings.R')

fileName <- paste0("multiOut_spID",speciesID,".rdata")
load(fileName)

# Multiout to variable
clusters <- multiOut

# species <- speciesNames[speciesID]
speciesNames <- colnames(pCROB)

estimatedName <- estimatedNames[estimatedID]

# Define variables
varXs <- c(11:13,17,18,30,43)
tabXsp<- data.table(melt(multiOut[,,4,,1]))
setnames(tabXsp,c("site","year","layer","species"))
tabXsp$species <- speciesNames[tabXsp$species]

tabX <- data.table()
for(i in varXs) {
  tabXx <- data.table(melt(multiOut[,,i,,1]))
  tabXx$variable <- varNames[i]
  tabXx <- merge(tabXx,tabXsp)
  tabX <- rbind(tabXx,tabX)
}


# Set varNames
varNames <- as.vector(unlist(dimnames(multiOut)[3]))

tabX$site <- as.factor(tabX$site)
tabX$layer <- as.factor(tabX$layer)
tabX$species <- as.factor(tabX$species)

# Set layer col values to integers
tabX$layer <- as.integer(tabX$layer)

# Remove non existent layers
tabX <- filter(tabX,layer<=nLayers[site])

plotsVars <- list(list())

layer_types <- c("trees", "clusters")
layerID <- 2

# Plot
for (siteX in 1:nSites) {
  for(variableX in varNames[varXs]){
    plotsVars[[variableX]] <- ggplot(data=tabX[variable==variableX & site==siteX],  aes(x=year,y=value,col=species, group=layer)) +
      geom_line() +
      ggtitle(paste0("Site ",siteX," ",layer_types[layerID]),variableX)
    
  }
  pdf_out <- paste0(layer_types[layerID],"_as_layers/site_",siteX)
  plot_path <- paste0("C:/Users/samu/Documents/yucatrote/r/forest_navigator23_r/data/plots/by_site/", pdf_out, ".pdf")
  pdf(plot_path)
  for(variableX in varNames[varXs]){
    print(plotsVars[[variableX]])
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
#   print(plotsVars[[variableX]])
# }

