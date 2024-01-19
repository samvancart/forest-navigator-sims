# Functions

# data_from: Where the weather inputs are from eg.eobs
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

# Residuals

get_residuals_pointplots <- function(plotsVars,variableX,siteX,tabX,data_from="", shape="species", col="layer") {
  # Convert to symbol in order to use aes with !! instead of deprecated aes_string
  if(!is.null(shape)){shape <- sym(shape)}
  if(!is.null(col)){col <- sym(col)}
  plotsVars[[variableX]] <- ggplot(data = tabX[site==siteX & variable==variableX], aes(x = year, y = residuals, shape=!!shape, col=!!col)) +
    geom_point() +
    ggtitle(paste0("Site ",siteX),variableX) +
    labs(tag=data_from)
  
  return(plotsVars)
}


get_residuals_boxplots <- function(plotsVars,variableX,siteX,tabX,data_from="", col="species", fill="layer") {
  # Convert to symbol in order to use aes with !! instead of deprecated aes_string
  if(!is.null(col)){col <- sym(col)}
  if(!is.null(fill)){fill <- sym(fill)}
  plotsVars[[variableX]] <- ggplot(data = tabX[site==siteX & variable==variableX], aes(y = residuals, col=!!col, fill=!!fill)) +
    geom_boxplot() +
    ggtitle(paste0("Site ",siteX),variableX) +
    labs(tag=data_from)
  
  return(plotsVars)
}



plot_residuals <- function(tabX, sub_folder, varNames, varXs, shape="species", point_col="layer", box_col="species", fill = "layer") {
  point_plots <- list(list())
  boxplots <- list(list())
  
  # Set nSites
  nSites <- max(as.integer(tabX$site))
  
  print(paste0("Creating ", sub_folder, " plots..."))
  
  # Plot
  for (siteX in 1:nSites) {
    for(variableX in varNames[varXs]){
      point_plots <- get_residuals_pointplots(point_plots,variableX,siteX,tabX, data_from, shape=shape, col=point_col)
      boxplots <- get_residuals_boxplots(boxplots,variableX,siteX,tabX, data_from, col=box_col, fill=fill)
      
    }
    plot_path <- get_by_site_plot_path("side_by_side",siteX, data_from, sub_folder = sub_folder)
    pdf(plot_path, width=14, height=7)
    for(variableX in varNames[varXs]){
      grid.arrange(point_plots[[variableX]],boxplots[[variableX]],ncol=2)
    }
    dev.off()
  }
  
  folder_path <- get_folder_path_from_plot_path(plot_path)
  print(paste0("Plots saved to ", folder_path))
}


plot_combined_residuals <- function(tabX, plot_path, formula=as.formula(paste("~", "species")), col="species", fill="species") {
  
  tabX$year <- as.factor(tabX$year)
  
  path <- paste0(plot_path,"combined.pdf")
  pdf(file=path)
  
  print("Plotting combined...")
  
  # Convert to symbol in order to use aes with !! instead of deprecated aes_string
  if(!is.null(col)){col <- sym(col)}
  if(!is.null(fill)){fill <- sym(fill)}
  
  for(varX in unique(tabX$variable)){
    p1 <- ggplot(data = tabX[variable==varX], aes(x = year, y = residuals, col=!!col)) +
      geom_boxplot()
    
    p2 <- ggplot(data = tabX[variable==varX], aes(x = residuals, fill=!!fill)) +
      geom_histogram(bins = 30) + 
      facet_wrap(formula)
    
    plotX <- ggarrange(p1,p2,nrow=2)
    
    plotX <-annotate_figure(plotX, top = text_grob(varX, face = "bold", size = 14))
    print(plotX)
  }
  dev.off()
  folder_path <- get_folder_path_from_plot_path(plot_path)
  print(paste0("Plots saved to ", folder_path))
}


get_shape_file_plot <- function(backgroundData,backgroundColour,topData,topColour,size) {
  plot <- ggplot() + 
    geom_sf(data = backgroundData, size = size, color = backgroundColour) +
    geom_sf(data = topData, size = size, color = topColour)
  
  return(plot)
}





# Paths

get_by_site_plot_path <- function(folder, siteX, data_from="gitlab", sub_folder="/") {
  file_path <- paste0(folder,sub_folder, data_from, "/site_", siteX)
  full_path <- paste0("data/plots/by_site/", file_path, ".pdf")
  
  return(full_path)
}

get_by_species_plot_path <- function(species, estimatedName, data_from="gitlab", managementName="managed") {
  file_path <- paste0(data_from, "/", species, "_plots_", estimatedName)
  full_path <- paste0("data/plots/by_species/", managementName , "/",file_path, ".pdf")

  return(full_path)
}

get_folder_path_from_plot_path <- function(plot_path) {
  c_idxs <- gregexpr("/", plot_path)
  end <- c_idxs[[1]][(length(c_idxs[[1]]))]
  folder_path <- substr(plot_path, start=0,stop=end)  
  
  return(folder_path)
}




