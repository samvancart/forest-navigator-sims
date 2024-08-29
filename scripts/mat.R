# Script for calculating MAT (mean annual temperature) ranges for mixture types 
# as described in section 3.1.2.1 of Initial forest states document.


source('scripts/settings.R')
source('./r/utils.R')



# Get nfi data
nfi_dt <- fread(paste0(config$PATH_nfi_sweden,"mix_types.csv"))

# CLIMATE IS FROM COMPARISON (21 sites)

# Get climate data
climate_dt <- fread(config$PATH_prebas_gitlab)

# Get nSites
nSites <- length(unique(climate_dt$climID))

# Get dt_nSites
dt_nSites <- data.table(get_df_nSites(nfi_dt, nSites))

# Add groupID to climate
climate_dt[,groupID := .GRP, by = siteID]

# Filter out unnecessary columns
tair_dt <- climate_dt[, c("time","tair","groupID")]

# Add year column
tair_dt$year <- format(tair_dt$time, format="%Y")

# Annual means for each groupID
tair_means_dt <- tair_dt[, lapply(.SD, "mean", na.rm = T), .SDcols = "tair", by = .(groupID, year)]


# Mix type column to use (either CORINE forest class or mixture type)
join_col <- "mixtype"
join_dt <- unique(dt_nSites[, c("groupID", ..join_col)])

# Join forest classes
joined_tair_means_dt <- left_join(tair_means_dt, join_dt, by = "groupID")

# Group as factor
joined_tair_means_dt$groupID <- as.factor(joined_tair_means_dt$groupID)

# Filter
filtered <- joined_tair_means_dt

# Remove outliers:
# Replace high outliers with max value from data with outliers removed
# Replace low outliers with min value from data with outliers removed
outliers_removed <- filtered %>%
  group_by(groupID) %>%
  mutate(upper = get_upperQ(pick(everything()), "tair"), lower = get_lowerQ(pick(everything()), "tair")) %>%
  mutate(outlier = tair < lower | tair > upper) %>%
  mutate(tair = ifelse((outlier & tair < lower), min(filter(pick(tair), !outlier)$tair), tair)) %>%
  mutate(tair = ifelse((outlier & tair > upper), max(filter(pick(tair), !outlier)$tair), tair)) %>%
  ungroup() %>%
  reframe(.,.[,(1:4)]) %>%
  as.data.table(.)


# Plot
ggplot(data = outliers_removed, mapping = aes(x = year, y = tair, group=groupID, fill=groupID)) +
  geom_boxplot()


# Get mat ranges
max_mat <- outliers_removed[, list(max_mat = max(tair)), by =  outliers_removed[, ..join_col]]
min_mat <- outliers_removed[, list(min_mat = min(tair)), by =  outliers_removed[, ..join_col]]
mat <- left_join(max_mat, min_mat, by = join_col)

# Join with nfi
nfi_mat <- left_join(nfi_dt, mat, by = join_col)










