source('scripts/settings.R')
source('./r/sample.R')
source('./r/netcdf.R')


# Data table where rows are sites and columns are days.
get_prebas_tran <- function(df, var) {
  
  siteIDs <- unique(df$siteID)
  all_wide <- data.table()
  
  for(id in siteIDs) {
    filtered <- filter(df,siteID==id)
    var_col <- filtered[var]
    df_wide <- as.data.frame(t(var_col))
    all_wide <- rbind(all_wide,df_wide)
  }
  
  return(all_wide)
}



path <- paste0("data/climate/extracted/eobs/leap_years_sampled_eobs.csv")
df <- as_tibble(read.csv(path, header=T))


prebas_df <- df[,c(1:4,7:8,12:14)]
prebas_df <- prebas_df %>%
  rename("precip"="rr",
         "tair"="tg",)


vars <- colnames(prebas_df[5:ncol(prebas_df)])

for (v in vars) {
  tran <- get_prebas_tran(prebas_df, v)
  path <- paste0("data/climate/extracted/eobs/tran/",v,".csv")
  print(paste0("Writing ",v,"..."))
  write.csv(tran, path, row.names = F)
  print("Done.")
}





