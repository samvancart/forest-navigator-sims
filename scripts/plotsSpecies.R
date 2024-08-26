source('scripts/settings.R')
source('./r/plots.R')
source('./r/utils.R')

# Functions

# Melt Lc
melt_lc <- function(lc,runID,siteType){
  melt_lc <- data.table(melt(lc),SiteType=siteType)
  colnames(melt_lc) <- c("site","year","value","SiteType")
  melt_lc$variable <- "Lc"
  melt_lc$runID <- runID
  return(melt_lc)
}




fileName <- paste0(rdata_path, "multiOut_spID",VAR_species_id,".rdata")
load(fileName)

# Settings variables
species <- get_speciesName(VAR_species_id, speciesDict)
estimatedName <- estimatedNames[estimatedID]
# estimated <- estimatedList[[estimatedID]]
managementName <- managementNames[managementID+1]


# Define variables
varXs <- c(11:14,17,18,30,43)

# Set varNames
varNames <- as.vector(unlist(dimnames(multiOut)[3]))

# Length of crown: H-Hc
lc_stN <- multiOut[,,11,,1] - multiOut[,,14,,1]
lc_st1 <- multiOut_st1[,,11,,1] - multiOut_st1[,,14,,1]
lc_st5 <- multiOut_st5[,,11,,1] - multiOut_st5[,,14,,1]



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


# Plot with shadow. # CHECK VARIABLE 'data_from' IN settings.R
for(variableX in varNames[varXs]){
  plotsVars <- get_shadow_plotsVars(plotsVars,variableX,tabX,data_from)
}


# plots to pdf
plot_path <- get_by_species_plot_path(species, estimatedName, data_from, managementName)

pdf(plot_path)
for(variableX in varNames[varXs]){
  print(plotsVars[[variableX]])
}
dev.off()

# print plots
for(variableX in varNames[varXs]){
  print(plotsVars[[variableX]])
}




# display all plots in grid
# grid.arrange(grobs = plotsVars)
# dev.off
