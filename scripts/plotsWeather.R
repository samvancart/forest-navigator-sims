source('scripts/settings.R')
source('./r/plots.R')

# Plot daily weather inputs by site

# Functions

build_long_format_weather_table <- function(tabX,varMatrix,varName) {
  tabX <- data.table(melt(varMatrix))
  tabX <- tabX[,1:3]

  tabX <- setNames(tabX, c("site", "day", "value"))
  
  tabX$day <- as.integer(tabX$day)
  tabX$variable <- varName
  
  return(tabX)
}



# Initialise tabX
tabX <- data.table()

# Variable matrices and names. CHECK VPD
varMatrices <- list(TAirtran,Preciptran,CO2tran,PARtran,VPDtran)
varNames <- c("tair","precip","co2","par","vpd")

# Get long format
for(i in 1:length(varMatrices)) {
  tabXx <- build_long_format_weather_table(tabXx, varMatrices[i], varNames[i])
  tabX <- rbind(tabX, tabXx)
}

tabX$site <- as.factor(tabX$site)
tabX$day <- as.factor(tabX$day)

nSites <- length(unique(tabX$site))

plotsVars <- list(list())


# Plot
for (siteX in 1:nSites) {
  for(variableX in varNames){
    plotsVars <- get_weather_plotsVars(plotsVars,variableX,siteX,tabX, data_from)
  }
  plot_path <- get_by_site_plot_path("weather_inputs",siteX, data_from)
  pdf(plot_path, width=14, height=7)

  for(variableX in varNames){
    print(plotsVars[[variableX]])
  }

  dev.off()

}




