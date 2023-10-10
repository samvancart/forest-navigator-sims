# Functions

get_plotsVars <- function(plotsVars,variableX,siteX,tabX,layerNumber) {
  plotsVars[[variableX]] <- ggplot(data=tabX[variable==variableX & site==siteX],  aes(x=year,y=value,col=species, group=layer)) +
    geom_line() +
    ggtitle(paste0("Site ",siteX," ",layerNames[layerNumber]),variableX)
  
  return(plotsVars)
}

get_sums_means_plotsVars <- function(plotsVars,variableX,siteX,tabX) {
  plotsVars[[variableX]] <- ggplot() +
    geom_line(data=tabX[variable==variableX & site==siteX],  aes(x=year,y=value,group=LayerType,col=LayerName)) +
    labs(col = "Layer Name") +
    ggtitle(paste0("Site ",siteX," "),variableX)
  
  return(plotsVars)
}

get_shadow_plotsVars <- function(plotsVars, variableX, tabX) {
  plotsVars[[variableX]] <- ggplot() +
    stat_summary(data=tabX[variable==variableX & runID %in% c("st1","st5")],aes(x=year,y=value),geom = "ribbon", fun.min = min, fun.max = max, alpha = 0.3, colour=NA) +
    geom_line(data=tabX[variable==variableX & runID %in% "N_based"],  aes(x=year,y=value,group=site,col=SiteType)) +
    ggtitle(species, variableX) +
    labs(caption = paste0(estimatedName," ", list(round(estimated,2))))
  
  return(plotsVars)
}







