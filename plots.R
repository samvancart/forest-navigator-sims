

source('settings.R')

fileName <- paste0("multiOut_spID",speciesID,".rdata")
load(fileName)

species <- speciesNames[speciesID]
out <- paste0("out_" ,speciesID,"_",defaultThin,"_",ClCut)
path <- paste0("C:/Users/samu/Documents/yucatrote/projects/forest-navigator23/data/csv/prebas_out/", out,".csv")
path

# define variables
varXs <- c(11:13,17,18,30,43)
# get siteTypes
sites <- data.table(melt(multiOut[,,3,1,1]))
# add siteTypes to table
tabX <- data.table(melt(multiOut[,,varXs,1,1]),SiteType=sites$value)

# write prebas output csv format: out_speciesID_defaultThin_ClCut
write.csv(tabX, path, row.names = F)

tabX$site <- as.factor(tabX$site)
tabX$SiteType <- as.factor(tabX$SiteType)
plotsVars <- list()

# Plot with shadow
for(variableX in varNames[varXs]){
  plotsVars[[variableX]] <- ggplot(data=tabX[variable==variableX],aes(x=year,y=value,col=SiteType,fill=SiteType)) +
    stat_summary(geom = "ribbon", fun.min = min, fun.max = max, alpha = 0.3, colour=NA) +
    stat_summary(geom = "line", fun = mean) +
    ggtitle(species, variableX)
}

pdf_out <- paste0(species,"_plots")
pdf_out
plot_path <- paste0("C:/Users/samu/Documents/yucatrote/projects/forest-navigator23/data/plots/", pdf_out, ".pdf")
pdf(plot_path)
for(variableX in varNames[varXs]){
  print(plotsVars[[variableX]])
}
dev.off()
for(variableX in varNames[varXs]){
  print(plotsVars[[variableX]])
}

