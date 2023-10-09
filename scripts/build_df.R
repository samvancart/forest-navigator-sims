source('scripts/settings.R')
source('./r/sample.R')
source('./r/netcdf.R')


# eobs_df <- get_netcdf(
#   eobs_path,
#   sites_path,
#   req_var = c("hu","qq","rr","tg","tn", "tx"),
#   lon_var = "lon",
#   lat_var = "lat",
#   time_var = "time",
#   coords = NULL)
# 
# print(eobs_df)
# print(sapply(eobs_df,anyNA))
# 
# # Fill missing values
# na_cols <- which(sapply(eobs_df,anyNA)==T)
# eobs_df[na_cols] <- na.approx(eobs_df[na_cols])
# 
# print(sapply(eobs_df,anyNA))
# 
# 
# # Par and vpd
# eobs_df["rss"] <- eobs_df["qq"]*0.0864
# eobs_df["par"] <- eobs_df["rss"]*0.44*4.56
# tair <- eobs_df$tn
# svp <- 610.7 * 10**(7.5*tair/(237.3+tair))
# eobs_df$vpd <- svp * (1-(eobs_df$hu/100)) / 1000
# 
# print(eobs_df)
# 
# # Sample simYears
# available_years <- unique(year(eobs_df$time))
# new_years <- 2023:2100
# 
# new_years_leap <- get_leap_years(new_years)
# new_years_non_leap <- get_non_leap_years(new_years)
# 
# available_years_leap <-  get_leap_years(available_years)
# available_years_non_leap <- get_non_leap_years(available_years)
# 
# sampled_leap <- get_samples_df(available_years_leap, new_years_leap)
# sampled_non_leap <- get_samples_df(available_years_non_leap, new_years_non_leap)
# 
# sampled <- rbind(sampled_leap,sampled_non_leap)
# sampled <- sampled[order(sampled)]
# 
# eobs_df_all <- bind_rows(apply(sampled,1,get_one_year_data,df=eobs_df))
# 
# sampled$check_is_equal <- apply(sampled,1,check_equal,df_all=eobs_df_all,df=eobs_df,cols=c(1,3:13))
# print(unique(sampled$check))
# 
# # Get co2
# co2_df <- read.csv("data/climate/provided/co2_annual_1850_2021.csv", header = T)
# co2_df$CO2[co2_df$year>=2017] <- co2_df$CO2[co2_df$year==2017]
# minYear <- 2022
# maxYear <- 2100
# years <- minYear:maxYear
# co2 <- rep(405.22,length(years))
# sim_co2 <- data.table(year = years, CO2 = co2)
# co2_df <- rbind(co2_df, sim_co2)
# years <- as.numeric(format(eobs_df_all$time,'%Y'))
# co2_vals <- sapply(years, function(x) co2_df$CO2[which(co2_df$year==x)])
# eobs_df_all$co2 <- co2_vals
# 
# print(eobs_df_all)
# print(tail(eobs_df_all))
# print(sapply(eobs_df_all,anyNA))

path <- paste0("data/climate/extracted/leap_years_sampled_eobs.csv")
# write.csv(eobs_df_all, path, row.names = F)

df <- as_tibble(read.csv(path, header=T))


filtered <- filter(df,siteID==7441480 & year(time)==1979)
filtered <- filter(df,year(time)==1979)
filtered<-filtered[,c(1,5)]
filtered[366,]

df_wide <- as_tibble(as.data.frame(t(filtered$hu)))
colnames(df_wide)
row.names(df_wide)

# MAKE TRAN FUNCTION


siteIDs<-unique(df$siteID)

all_wide <- data.table()
for(id in siteIDs) {
  filtered <- filter(df,siteID==id)
  filtered <- filtered[,c(1,5)]
  df_wide <- as.data.frame(t(filtered$hu))
  all_wide <- rbind(all_wide,df_wide)
}
all_wide <- as_tibble(all_wide)




