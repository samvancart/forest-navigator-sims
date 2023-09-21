

source('settings.R')

fileName <- paste0("multiOut_spID",speciesID,".rdata")
load(fileName)

# species <- speciesNames[speciesID]
speciesNames <- colnames(pCROB)

out <- paste0("out_" ,speciesID,"_",defaultThin,"_",ClCut)
path <- paste0("C:/Users/samu/Documents/yucatrote/projects/forest-navigator23/data/csv/prebas_out/", out,".csv")

estimatedName <- estimatedNames[estimatedID]

# define variables
varXs <- c(11:13,17,18,30,43)
# get siteTypes
# sites <- data.table(melt(multiOut[,,3,1,1]))
# add siteTypes to table
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

# tabX <- data.table(melt(multiOut[,,varXs,,1]))

# tabX <- data.table(melt(multiOut_st1[,,varXs,1,1]),SiteType=1); tabX$runID ="st1"
# tabX <- rbind(tabX,data.table(melt(multiOut_st5[,,varXs,1,1]),SiteType=5,runID ="st5"))

# tabXsp<- data.table(melt(multiOut[,,4,,1]))
# setnames(tabXsp,c("site","year","layer","speciesID"))
# tabXvars<- data.table(melt(multiOut[,,varXs,1,1]))
# tabXestimated <- merge(tabXst,tabXvars)
# tabXestimated$runID <- "N_based"
# tabX <- rbind(tabX,tabXestimated)


# tabXx <- merge(tabXsp,tabX,by=c("site","year","layer"))



# Set varNames
varNames <- as.vector(unlist(dimnames(multiOut)[3]))

# write prebas output csv format: out_speciesID_defaultThin_ClCut
write.csv(tabX, path, row.names = F)

tabX$site <- as.factor(tabX$site)
tabX$layer <- as.factor(tabX$layer)
tabX$species <- as.factor(tabX$species)
# tabX$runID <- as.factor(tabX$runID)
plotsVars <- list(list())

# Choose 1 site
for(variableX in varNames[varXs]){
  plotsVars[[variableX]] <- ggplot() +
    geom_line(data=tabX[variable==variableX & site==4],  aes(x=year,y=value,col=species, group=layer)) +
    ggtitle(variableX)
}



# # Print all
# for (siteX in 1:nSites) {
#   for(variableX in varNames[varXs]){
#     plotsVars[[variableX]] <- ggplot() +
#       # stat_summary(data=tabX[variable==variableX & runID %in% c("st1","st5")],aes(x=year,y=value),geom = "ribbon", fun.min = min, fun.max = max, alpha = 0.3, colour=NA) +
#       geom_line(data=tabX[variable==variableX & site==siteX],  aes(x=year,y=value,col=species, group=layer)) +
#       ggtitle(siteX,variableX)
#     # labs(caption = paste0(estimatedName," ", list(round(estimated,2))))
#     print(plotsVars[[variableX]])
#   }
# }



# display all plots in grid
# grid.arrange(grobs = plotsVars)
# dev.off

# plots to pdf
pdf_out <- paste0(species,"_plots_", estimatedName)
pdf_out
plot_path <- paste0("C:/Users/samu/Documents/yucatrote/r/forest_navigator23_r/data/plots/", pdf_out, ".pdf")
pdf(plot_path)
for(variableX in varNames[varXs]){
  print(plotsVars[[variableX]])
}
dev.off()

# print plots
for(variableX in varNames[varXs]){
  print(plotsVars[[variableX]])
}

