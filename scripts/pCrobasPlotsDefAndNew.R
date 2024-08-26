source('scripts/settings.R') 


# Functions

plot_initPrebas <- function(def_multiOut, new_multiOut, varXs, species, yieldTabs=NULL, y_units="", nfi_dt=NULL, nfi_vars=NULL) {
  par(mfrow=c(2,1))
  for(i in 1:length(varXs)) {
    varX <- varXs[i]
    y_limit <- 45
    
    if (!is.null(nfi_dt)) {
      nfi_var <- nfi_vars[i]
      y_limit <- max(nfi_dt[, ..nfi_var])
    }
    
    y_unit <- y_units[as.character(varX)][[1]]
    ylim <- range(y_limit, def_multiOut[,,varX,1,1], new_multiOut[,,varX,1,1])
    plot(def_multiOut[1,,7,1,1], def_multiOut[1,,varX,1,1],col=1,type='l',
         main=paste0(varNames[varX], " ", species), ylim=ylim, xlab="years", ylab=y_unit)
    legend("topleft", legend=c("Def", "New", "Nfi", "Yield tables"), fill=c(1, 7, 2, 4), cex=0.7, box.lty=0)
    if (!is.null(nfi_dt)) {
      points(nfi_dt[,8][[1]], nfi_dt[, ..nfi_var][[1]], col= alpha(2, 0.2), pch=10, cex=0.5)
    }
    for(i in 2:nSites) lines(def_multiOut[i,,7,1,1], def_multiOut[i,,varX,1,1],col=1)
    for(i in 2:nSites) lines(new_multiOut[i,,7,1,1], new_multiOut[i,,varX,1,1],col=7)

    if(species=='Beech'){
      if (varX==11) points(age_yieldTab,h_yieldTab, col=4, pch=20); if (varX==12) points(age_yieldTab,d_yieldTab, col=4, pch=20); abline(h=50)
    }
    
  }
}

# NFI DATA
nfi_path <- VAR_nfi_sweden_paths[VAR_layer_id]
df <- fread(nfi_path)
dt <- as.data.table(df)

# varXs <- c(11:13,17)
varXs <- c(11,12)
nfi_vars <- c(7,6)

y_units <- c("11"="m", "12"="cm", "13"="m²/ha", "17"="N")
plot_path <- "data/plots/pCrobasDefNew/"


fileName <- paste0(rdata_path, "initPrebas_Beech.rdata")
load(fileName)

nSites <- initPrebas$nSites
initPrebas$pPRELES <- pPRELESfasy  #modify preles parameters for Fagus
initPrebas$pCROBAS <- pCROB        ### update default crobas parameters
initPrebas$multiInitVar[,2,] <- 12 ### update age
run_def <- multiPrebas(initPrebas)  ###run with fagus romanian calibration

initPrebas$multiInitVar[,1,] <- 12  ###update species ID
initPrebas$pCROBAS[31, 12] <- VAR_theta_max # ThetaMax
run_new <- multiPrebas(initPrebas) ####run with fagus boreal calibration

nfi_dt <- dt[speciesID==4] # fagus=4 in NFI

####data from yield tables
h_yieldTab <- c(13,25,28)
age_yieldTab <- c(45,90,120)
d_yieldTab <- c(13,29,43)

## All
pdf_path <- paste0(plot_path,"All_nfi_thetaMax_", VAR_theta_max,".pdf")
pdf(pdf_path, width = 14, height = 9)


# pdf_path <- paste0(plot_path,"Beech.pdf")
# pdf(pdf_path)


# Create plots
plot_initPrebas(run_def$multiOut, run_new$multiOut, varXs, 
                'Beech', yieldTabs=c(h_yieldTab, age_yieldTab, d_yieldTab), y_units = y_units, nfi_dt = nfi_dt, nfi_vars = nfi_vars)



# points(dt[speciesID==4]$Age, dt[speciesID==4]$Dbh, col=2,pch=20)
# dev.off()

####Spruce analysis
# rm(list=ls()); gc()

fileName <- paste0(rdata_path, "initPrebas_Spruce.rdata")
load(fileName)

nSites <- initPrebas$nSites

initPrebas$pCROBAS <- pCROB        ### update default crobas parameters
initPrebas$multiInitVar[,2,] <- 12 ### update age
run_def <- multiPrebas(initPrebas)  ###run with fagus romanian calibration

initPrebas$pCROBAS[17,2] <- initPrebas$pCROBAS[17,2] * 1.3
initPrebas$pCROBAS[31,2] <- VAR_theta_max # ThetaMax
run_new <- multiPrebas(initPrebas) ####run with fagus boreal calibration

nfi_dt <- dt[speciesID==2] # fagus=4 in NFI

# pdf_path <- paste0(plot_path,"Spruce.pdf")
# pdf(pdf_path)

# Create plots
plot_initPrebas(run_def$multiOut, run_new$multiOut, varXs, 'Spruce', y_units = y_units, nfi_dt = nfi_dt, nfi_vars = nfi_vars)

# dev.off()


####Pine analysis
# rm(list=ls()); gc()

fileName <- paste0(rdata_path, "initPrebas_Pine.rdata")
load(fileName)

nSites <- initPrebas$nSites

initPrebas$pCROBAS <- pCROB        ### update default crobas parameters
initPrebas$multiInitVar[,2,] <- 12 ### update age
run_def <- multiPrebas(initPrebas)  ###run with fagus romanian calibration

initPrebas$pCROBAS[17,1] <- initPrebas$pCROBAS[17,1] * 1.3
initPrebas$pCROBAS[31, 1] <- VAR_theta_max # ThetaMax
run_new <- multiPrebas(initPrebas) ####run with fagus boreal calibration

nfi_dt <- dt[speciesID==1] # fagus=4 in NFI

# pdf_path <- paste0(plot_path,"Pine.pdf")
# pdf(pdf_path)

# Create plots
plot_initPrebas(run_def$multiOut, run_new$multiOut, varXs, 'Pine', y_units = y_units, nfi_dt = nfi_dt, nfi_vars = nfi_vars)

# dev.off()



####Birch analysis
# rm(list=ls()); gc()

fileName <- paste0(rdata_path, "initPrebas_Birch.rdata")
load(fileName)

nSites <- initPrebas$nSites

initPrebas$pCROBAS <- pCROB        ### update default crobas parameters
initPrebas$multiInitVar[,2,] <- 12 ### update age
run_def <- multiPrebas(initPrebas)  ###run with fagus romanian calibration

initPrebas$pCROBAS[17,3] <- initPrebas$pCROBAS[17,3] * 0.6
initPrebas$pCROBAS[31,3] <- VAR_theta_max # ThetaMax
run_new <- multiPrebas(initPrebas) ####run with fagus boreal calibration

nfi_dt <- dt[speciesID==3] # fagus=4 in NFI

# pdf_path <- paste0(plot_path,"Birch.pdf")
# pdf(pdf_path)

# Create plots
plot_initPrebas(run_def$multiOut, run_new$multiOut, varXs, 'Birch', y_units = y_units, nfi_dt = nfi_dt, nfi_vars = nfi_vars)

dev.off()


