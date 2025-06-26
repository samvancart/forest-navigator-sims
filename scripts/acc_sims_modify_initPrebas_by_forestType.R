library(Rprebasso)
library(data.table)


initPrebas <- readRDS("C:/Users/checc/Documents/yucatrote/forestNavigator/ForestManagement/initPrebas_PREBAS_7545040_historical_bau.rds")

#run the original data, just for testing, this part should be removed##
original_run <- multiPrebas(initPrebas)
##----##

####those lines should be removed when the initPrebas is created with the last version of the model
initPrebas$ftTapioPar <- ftTapio
initPrebas$tTapioPar <- tTapio
for(rrr in 1:initPrebas$nSites) initPrebas$thinning[rrr,,3] <- (initPrebas$nLayers[rrr] - 3 + 1):initPrebas$nLayers[rrr]
###-----###

####Sam, you need to check all code and the files that I'm using should be replaced
forestTypes_tab <- readRDS("data/acc/docs/management/test_sites/siteID-lookup_7545040.rds")
forest_management <- fread(sweden_man_path)
forest_management$forest_type <- sub("_[0-9].*$", "", forest_management$ForestTypeElevSite)
forest_management$for_man <- forest_management$`BAU-Mgt1`
#-----#

# in this example there are only standard management sites. I generated some fake data to test the code, this part should be removed, and the code tested with additional data 
forestTypes_tab$forest_type[c(37,28,1,8,3)] <- "SWE_PopTr"
forestTypes_tab$forest_type[c(4,2,33,11,29)] <- "SWE_AlnGl"
forestTypes_tab$forest_type[c(21,40,9,24,38)] <- "SWE_QueSp"
forestTypes_tab$forest_type[c(5,21,35,23,20)] <- "SWE_PinCo"
forestTypes_tab$forest_type[c(25,36,39,14,18)] <- "SWE_FagSy"
#-----#

###I do not know why I get duplicates, this is why I use unique(), please double check and find an alternative solution
setkey(forest_management,"forest_type")
setkey(forestTypes_tab,"forest_type")
forest_type_management_tab <- unique(merge(forestTypes_tab,forest_management[,.(forest_type,for_man)],by="forest_type",allow.cartesian=FALSE))
##----##
merge(forestTypes_tab,forest_management[,.(forest_type,for_man)],by="forest_type",allow.cartesian=FALSE)

forest_management[grepl("SWE_MIX03", ForestTypeElevSite)]


## update the initialization##
initPrebas <- forest_management_update(initPrebas = initPrebas, forest_type_management_tab = forest_type_management_tab,
                                       country="Sweden", management="BAU")
##----##

##run the model##
new_run <- multiPrebas(initPrebas)
##---##

##randmly make some plots for testing##
##### I rerun this just to make the plots
pop_sites <- sort(forest_type_management_tab$site[which(forest_type_management_tab$for_man == "PopTr_CC")])
alnus_sites <- sort(forest_type_management_tab$site[which(forest_type_management_tab$for_man == "AlnSp_CC")])
quercus_sites <- sort(forest_type_management_tab$site[which(forest_type_management_tab$for_man == "QueSp_SW")])
pinco_sites <- sort(forest_type_management_tab$site[which(forest_type_management_tab$for_man == "PinCo_CC")])
fagus_sites <- sort(forest_type_management_tab$site[which(forest_type_management_tab$for_man == "FagSy_SW")])
##----##


modifiedSites <- c(pop_sites,alnus_sites,quercus_sites,pinco_sites,fagus_sites)
originalSites <- setdiff(1:40, modifiedSites)

siteX <- sample(modifiedSites,1)
varX <- 13
layesX=6
plot(rowSums(original_run$multiOut[siteX,,varX,,1]))
points(rowSums(new_run$multiOut[siteX,,varX,,1]),col=2,pch=20)

siteX <- sample(originalSites,1)
varX <- 13
layesX=6
plot(rowSums(original_run$multiOut[siteX,,varX,,1]))
points(rowSums(new_run$multiOut[siteX,,varX,,1]),col=2,pch=20)
##----##