source('scripts/settings.R')
source(config$PATH_acc_sims_prepare_init_settings)



plgid <- 8149995
clim_scen <- "historical"

base_path <- file.path(clean_data_base_path, paste0("plgid_", plgid))
base_clim_path <- file.path(base_path, "climate")
clim_dirs <- list.files(base_clim_path)
clim_dir_paths <- file.path(base_clim_path, clim_dirs)
clim_dir_path <- grep("historical", clim_dir_paths, value = TRUE)

clim_file_paths <- list.files(clim_dir_path, full.names = T)

parTran <- loadRDataFile(file.path(base_clim_path, clim_scen, "parTran.rdata"))
co2Tran <- loadRDataFile(file.path(base_clim_path, clim_scen, "co2Tran.rdata"))
tairTran <- loadRDataFile(file.path(base_clim_path, clim_scen, "tairTran.rdata"))
vpdTran <- loadRDataFile(file.path(base_clim_path, clim_scen, "vpdTran.rdata"))
precipTran <- loadRDataFile(file.path(base_clim_path, clim_scen, "precipTran.rdata"))





siteInfo <- loadRDataFile(file.path(base_path, "site", "siteInfo.rdata"))
multiInitVar <- loadRDataFile(file.path(base_path, "tree_data", "multiInitVar.rdata"))

nYears <- get_nYears_from_acc_tran(clim_dir_path)
nSites <- nrow(siteInfo)



print("Initialising model...")
initPrebas <- InitMultiSite(nYearsMS = rep(nYears,nSites),
                            siteInfo = as.matrix(siteInfo),
                            multiInitVar = multiInitVar,
                            PAR = parTran,
                            VPD = vpdTran,
                            CO2= co2Tran,
                            Precip=precipTran,
                            TAir=tairTran,
                            CO2model = 1)

print("Done.")




modOut <- regionPrebas(initPrebas)

base_output_path <- "data/acc/output/test_sites"
save_path <- file.path(base_output_path, paste0("plgid_", plgid), "modout.rdata")
save(modOut, file = save_path)











































