source('scripts/settings.R')
source(config$PATH_acc_sims_prepare_init_settings)



plgid <- as.integer(7998833)
clim_scen <- "historical"


####management parameters
defaultThin = 0
ClCut = 0
mortMod = 3
ingrowth = T

init_args <- list(plgid = plgid, clim_scen = clim_scen, clean_data_base_path = clean_data_base_path)
man_init_args <- list(defaultThin = defaultThin,
                      ClCut = ClCut,
                      mortMod = mortMod,
                      ingrowth = ingrowth)

initPrebas <- do.call(get_init_prebas_for_plgid, c(init_args, man_init_args))


modOut <- get_modOut(regionPrebas, initPrebas)


multiOut <- modOut$multiOut




#!!!!!issues: 
# PAR units is wrong
# first column seems to be an ID 
# are the leap years processed correctly?
# 
# parTran[,1]
# 
# parTran <- parTran[,2:26585]*100  
# vpdTran <- vpdTran[,2:26585]
# co2Tran <- co2Tran[,2:26585]
# precipTran <- precipTran[,2:26585]
# tairTran <- tairTran[,2:26585]


#### foroutput processing
varOutID <- c(44,18,19,11:13,17,30,43,42,37,7,22,31:33,24:25,47,50)
vHarv <- c(30,2)

# harv  ###check over bark  and tree tops are included?
#transp ####we have ET not just transp
###branch biomass?separately or included in the stem?
stem_biom_sap
stem_biom_heart


modOut <- get_modOut(regionPrebas, initPrebas)

# modOut2 <- regionPrebas(initPrebas)


multiOut <- modOut$multiOut

out_dt <- as.data.table(melt(multiOut[,,varOutID,,1]))
species <- as.data.table(melt(multiOut[,,4,,1]))
v_harv <- as.data.table(melt(multiOut[,,30,,2]))

setnames(species, old = "value", new = "species")
out_dt_species <- merge(out_dt, species)
v_harv_species <- merge(v_harv, species)
v_harv_species[, variable := "harv"]
out_dt_all <- rbind(out_dt_species, v_harv_species)

conversions_dt <- fread("data/acc/docs/forest_nav_conversions_lookup.csv")


varNames
unique(out_dt_all$variable)
unique(conversions_dt$Variable)

out_dt_all[, Model := "PREBAS"]
out_dt_all[, Country := "Finland"]
out_dt_all[, Climate_scenario := clim_scen]
out_dt_all[, Management_scenario := "noman"]



base_output_path <- "data/acc/output/test_sites"
init_save_path <- file.path(base_output_path, paste0("plgid_", plgid), "initPrebas.rdata")
modOut_save_path <- file.path(base_output_path, paste0("plgid_", plgid), "modout.rdata")
multiOut_save_path <- file.path(base_output_path, paste0("plgid_", plgid), "multiOut.rdata")


print(paste0("Saving initPrebas into ", init_save_path))
save(initPrebas, file = init_save_path)
print("Done.")

print(paste0("Saving modOut into ", modOut_save_path))
save(modOut, file = modOut_save_path)
print("Done.")


print(paste0("Saving multiOut into ", multiOut_save_path))
save(multiOut, file = multiOut_save_path)
print("Done.")







































