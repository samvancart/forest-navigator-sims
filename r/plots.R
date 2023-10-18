# Functions

# data_from: Where weather inputs are from eg.eobs
get_plotsVars <- function(plotsVars,variableX,siteX,tabX,layerNumber, data_from="") {
  plotsVars[[variableX]] <- ggplot(data=tabX[variable==variableX & site==siteX],  aes(x=year,y=value,col=species, group=layer)) +
    geom_line() +
    ggtitle(paste0("Site ",siteX," ",layerNames[layerNumber]),variableX) +
    labs(tag=data_from)
  
  return(plotsVars)
}

get_sums_means_plotsVars <- function(plotsVars,variableX,siteX,tabX, data_from="") {
  plotsVars[[variableX]] <- ggplot() +
    geom_line(data=tabX[variable==variableX & site==siteX],  aes(x=year,y=value,group=LayerType,col=LayerName)) +
    labs(tag=data_from, col = "Layer Name") +
    ggtitle(paste0("Site ",siteX," "),variableX)
  
  return(plotsVars)
}

get_shadow_plotsVars <- function(plotsVars, variableX, tabX, data_from="") {
  plotsVars[[variableX]] <- ggplot() +
    stat_summary(data=tabX[variable==variableX & runID %in% c("st1","st5")],aes(x=year,y=value),geom = "ribbon", fun.min = min, fun.max = max, alpha = 0.3, colour=NA) +
    geom_line(data=tabX[variable==variableX & runID %in% "N_based"],  aes(x=year,y=value,group=site,col=SiteType)) +
    ggtitle(species, variableX) +
    labs(tag=data_from, caption = paste0(estimatedName," ", list(round(estimated,2))))

  
  return(plotsVars)
}

get_weather_plotsVars <- function(plotsVars,variableX,siteX,tabX,data_from="") {
  plotsVars[[variableX]] <- ggplot(data=tabX[variable==variableX & site==siteX],aes(x=day,y=value,group=variable)) +
    geom_line() +
    ggtitle(paste0("Site ",siteX," "),variableX) +
    labs(tag=data_from)
  
  return(plotsVars)
}



get_by_site_plot_path <- function(folder, siteX, data_from="gitlab", sub_folder="/") {
  file_path <- paste0(folder,sub_folder, data_from, "/site_", siteX)
  full_path <- paste0("data/plots/by_site/", file_path, ".pdf")
  
  return(full_path)
}

get_by_species_plot_path <- function(species, estimatedName, data_from="gitlab") {
  file_path <- paste0(data_from, "/", species, "_plots_", estimatedName)
  full_path <- paste0("data/plots/by_species/", file_path, ".pdf")

  return(full_path)
}

get_folder_path_from_plot_path <- function(plot_path) {
  c_idxs <- gregexpr("/", plot_path)
  end <- c_idxs[[1]][(length(c_idxs[[1]]))]
  folder_path <- substr(plot_path, start=0,stop=end)  
  
  return(folder_path)
}




