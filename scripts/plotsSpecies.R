source('scripts/settings.R')
source('./r/plots.R')

fileName <- paste0(rdata_path, "multiOut_spID",speciesID,".rdata")
load(fileName)

species <- speciesNames[speciesID]

estimatedName <- estimatedNames[estimatedID]
estimated <- estimatedList[[estimatedID]]

# define variables
varXs <- c(11:14,17,18,30,43)

# Length of crown: H-Hc
lc_stN <- multiOut[,,11,,1] - multiOut[,,14,,1]
lc_st1 <- multiOut_st1[,,11,,1] - multiOut_st1[,,14,,1]
lc_st5 <- multiOut_st5[,,11,,1] - multiOut_st5[,,14,,1]

# Melt Lc
melt_lc <- function(lc,runID,siteType){
  melt_lc <- data.table(melt(lc),SiteType=siteType)
  colnames(melt_lc) <- c("site","year","value","SiteType")
  melt_lc$variable <- "Lc"
  melt_lc$runID <- runID
  return(melt_lc)
}

# Build long format table
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


# Lc to varXs
varXs <- append(varXs,55)

# # Add Lc to names
varNames[55] = "Lc"


# Column as factor
tabX$site <- as.factor(tabX$site)
tabX$SiteType <- as.factor(tabX$SiteType)
tabX$runID <- as.factor(tabX$runID)
plotsVars <- list()


# Plot with shadow
for(variableX in varNames[varXs]){
  plotsVars <- get_shadow_plotsVars(plotsVars,variableX,tabX)
}

# display all plots in grid
# grid.arrange(grobs = plotsVars)
# dev.off

# plots to pdf
pdf_out_eobs <- paste0("eobs/", species, "_plots_", estimatedName)
pdf_out_gitlab <- paste0("gitlab/", species, "_plots_", estimatedName)


plot_path <- paste0("data/plots/by_species/", pdf_out_eobs, ".pdf")
# plot_path <- paste0("data/plots/by_species/", pdf_out_gitlab, ".pdf")

pdf(plot_path)
for(variableX in varNames[varXs]){
  print(plotsVars[[variableX]])
}
dev.off()

# print plots
for(variableX in varNames[varXs]){
  print(plotsVars[[variableX]])
}
