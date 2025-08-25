source('scripts/settings.R')
source(config$PATH_acc_sims_prepare_init_settings)

plgid <- 7998833
clim_scen <- "historical"

base_output_path <- "data/acc/output/test_sites"
modOut_load_path <- file.path(base_output_path, paste0("plgid_", plgid), "modout.rdata")
multiOut_load_path <- file.path(base_output_path, paste0("plgid_", plgid), "multiOut.rdata")

load(multiOut_load_path)







baWmean_multiOut <- function (multiOut, varX) {
  
  weightXs <- apply(multiOut[, , 13, , 1], 1:2, 
                    FUN = function(vec) vec/sum(vec, na.rm = T))
  weightXs <- aperm(weightXs, c(2:3, 1))
  weightXs[which(is.na(weightXs))] <- 0
  weigthedMean <- apply(multiOut[, , varX, , 1] * 
                          weightXs, 1:2, sum, na.rm = T)
  return(weigthedMean)
}







varOuts <- c("NEP/SMI[layer_1]","GPPtrees", "npp", "grossGrowth/bb BA disturbed", 
             "soilC", "V", "age", "WroundWood","VroundWood",
             "Litter_fol", "Litter_fr", 
             "Litter_fWoody", "Litter_cWoody",
             "DeadWoodVolume", "D", "BA", "H", "Vmort","Wdb",
             "Hc_base","wf_STKG","Rh")
if(!exists("varSel")){
  varSel <- match(varOuts,varNames)
}
if(!exists("specialVars")){
  specialVars <- c("domSpecies","domAge","Vdec","Vpine","Vspruce","VenergyWood",
                   "WenergyWood","Wtot","GVgpp","GVw")
}

#varSel <- c(7,8,9,11:13,17:18,22,24:33,37:39,41:46)   #### variables IDs to be stored

###set if you want to use Layers sum of BA average of stored variables
if(!exists("funX")){
  funX <- rep("sum",length(varSel))
  funX[match(varNames[c(7,11:12,14)],varNames[varSel])] <- "baWmean"
}


vars_list <- list()
for (ij in 1:length(varSel)) {
  marginX= 1:2#(length(dim(out$annual[,,varSel,]))-1)
  print(paste0("varSel ", varSel[ij]))
  if(funX[ij]=="baWmean"){
    outX <- data.table(baWmean_multiOut(multiOut,varSel[ij]))
  }
  if(funX[ij]=="sum"){
    outX <- data.table(apply(multiOut[,,varSel[ij],,1],marginX,sum))
  }
  
  # Remove special characters from varNames[varSel[ij]]
  varSel_name <- strsplit(varNames[varSel[ij]], split = "/")[[1]][1]
  assign(varSel_name,outX)
  vars_list[[varSel_name]] <- outX
}







v1 <- vars_list$H
v1[, site := paste("site", 1:.N)]
melted <- melt.data.table(v1, variable.name = "year", value.name = "value")
melted[, site := seq_len(.N), by = "year"]
melted[, year := as.factor(year)]


p <- ggplot(melted, aes(x = year, y = value, group = site, color = site)) +
  geom_line() +
  labs(title = "Values Over Years for Different Sites", x = "Year", y = "Value") +
  theme_minimal()

# Print the plot
print(p)














