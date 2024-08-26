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

print(paste0("Running plotsWeather.R"))

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

print(paste0("Creating plots..."))

# Plot
# CHECK VARIABLE 'config$VAR_data_from' IN settings.R
for (siteX in 1:nSites) {
  for(variableX in varNames){
    plotsVars <- get_weather_plotsVars(plotsVars,variableX,siteX,tabX, config$VAR_data_from)
  }
  plot_path <- get_by_site_plot_path("weather_inputs",siteX, config$VAR_data_from)
  pdf(plot_path, width=14, height=7)

  for(variableX in varNames){
    print(plotsVars[[variableX]])
  }

  dev.off()

}

folder_path <- get_folder_path_from_plot_path(plot_path)
print(paste0("Plots saved to ", folder_path))


