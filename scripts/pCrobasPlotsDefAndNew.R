source('scripts/settings.R') 


# Functions

plot_initPrebas <- function(def_multiOut, new_multiOut, varXs, species, yieldTabs=NULL, y_units="") {
  par(mfrow=c(2,2))
  for(varX in varXs) {
    y_unit <- y_units[as.character(varX)][[1]]
    ylim <- range(45, def_multiOut[,,varX,1,1], new_multiOut[,,varX,1,1])
    plot(def_multiOut[1,,7,1,1], def_multiOut[1,,varX,1,1],col=1,type='l',
         main=paste0(varNames[varX], " ", species), ylim=ylim, xlab="years", ylab=y_unit)
    legend("bottom", legend=c("Def", "New"), fill=c(1, 7), cex=0.7, box.lty=0)
    for(i in 2:nSites) lines(def_multiOut[i,,7,1,1], def_multiOut[i,,varX,1,1],col=1)
    for(i in 2:nSites) lines(new_multiOut[i,,7,1,1], new_multiOut[i,,varX,1,1],col=7)
    if(species=='Beech'){
      if (varX==11) points(age_yieldTab,h_yieldTab); if (varX==12) points(age_yieldTab,d_yieldTab); abline(h=50)
    }
  }
}



varXs <- c(11:13,17)
y_units <- c("11"="m", "12"="cm", "13"="m²/ha", "17"="N")


fileName <- paste0(rdata_path, "initPrebas_Beech.rdata")
load(fileName)

nSites <- initPrebas$nSites
initPrebas$pPRELES <- pPRELESfasy  #modify preles parameters for Fagus
initPrebas$pCROBAS <- pCROB        ### update default crobas parameters
initPrebas$multiInitVar[,2,] <- 12 ### update age
run_def <- multiPrebas(initPrebas)  ###run with fagus romanian calibration

initPrebas$multiInitVar[,1,] <- 12  ###update species ID
run_new <- multiPrebas(initPrebas) ####run with fagus boreal calibration

####data from yield tables
h_yieldTab <- c(13,25,28)
age_yieldTab <- c(45,90,120)
d_yieldTab <- c(13,29,43)

# Create plots
plot_initPrebas(run_def$multiOut, run_new$multiOut, varXs, 
                'Beech', yieldTabs=c(h_yieldTab, age_yieldTab, d_yieldTab), y_units = y_units)


####Spruce analysis
# rm(list=ls()); gc()

fileName <- paste0(rdata_path, "initPrebas_Spruce.rdata")
load(fileName)

nSites <- initPrebas$nSites

initPrebas$pCROBAS <- pCROB        ### update default crobas parameters
initPrebas$multiInitVar[,2,] <- 12 ### update age
run_def <- multiPrebas(initPrebas)  ###run with fagus romanian calibration

initPrebas$pCROBAS[17,2] <- initPrebas$pCROBAS[17,2] * 1.3
run_new <- multiPrebas(initPrebas) ####run with fagus boreal calibration

# Create plots
plot_initPrebas(run_def$multiOut, run_new$multiOut, varXs, 'Spruce', y_units = y_units)



####Pine analysis
# rm(list=ls()); gc()

fileName <- paste0(rdata_path, "initPrebas_Pine.rdata")
load(fileName)

nSites <- initPrebas$nSites

initPrebas$pCROBAS <- pCROB        ### update default crobas parameters
initPrebas$multiInitVar[,2,] <- 12 ### update age
run_def <- multiPrebas(initPrebas)  ###run with fagus romanian calibration

initPrebas$pCROBAS[17,1] <- initPrebas$pCROBAS[17,1] * 1.3
run_new <- multiPrebas(initPrebas) ####run with fagus boreal calibration

# Create plots
plot_initPrebas(run_def$multiOut, run_new$multiOut, varXs, 'Pine', y_units = y_units)



####Birch analysis
# rm(list=ls()); gc()

fileName <- paste0(rdata_path, "initPrebas_Birch.rdata")
load(fileName)

nSites <- initPrebas$nSites

initPrebas$pCROBAS <- pCROB        ### update default crobas parameters
initPrebas$multiInitVar[,2,] <- 12 ### update age
run_def <- multiPrebas(initPrebas)  ###run with fagus romanian calibration

initPrebas$pCROBAS[17,3] <- initPrebas$pCROBAS[17,3] * 0.6
run_new <- multiPrebas(initPrebas) ####run with fagus boreal calibration

# Create plots
plot_initPrebas(run_def$multiOut, run_new$multiOut, varXs, 'Birch', y_units = y_units)



