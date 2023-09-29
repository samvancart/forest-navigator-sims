source('settings.R')

get_plotsVars <- function(plotsVars,variableX,siteX,tabX) {
  plotsVars[[variableX]] <- ggplot(data=tabX[variable==variableX & site==siteX],aes(x=day,y=value,group=variable)) +
    geom_line() +
    ggtitle(paste0("Site ",siteX," "),variableX)
  
  return(plotsVars)
}



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

# Variable matrices and names
varMatrices <- list(TAirtran,Preciptran,CO2tran,PARtran,VPDtran_kpa)
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
    plotsVars <- get_plotsVars(plotsVars,variableX,siteX,tabX)
  }
  # f_name <- paste0("weather_inputs/site_",siteX)
  # plot_path <- paste0("C:/Users/samu/Documents/yucatrote/r/forest_navigator23_r/data/plots/by_site/", f_name, ".pdf")
  # pdf(plot_path, width=14, height=7)
  # 
  # grid.arrange(grobs = plotsVars)
  # for(variableX in varNames){
  #   print(plotsVars[[variableX]])
  # }
  # 
  # dev.off()

}




