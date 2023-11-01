source('scripts/settings.R')
source('./r/utils.R')


# Calculates MAT (mean annual temperature) ranges for mixture types


# Get nfi data
nfi_df <- fread(paste0(nfi_sweden_path,"mix_types.csv"))

# Get climate data
climate_df <- fread(prebas_gitlab_path)
class(climate_df)

# Get nSites
nSites <- length(unique(climate_df$climID))

# Get df_nSites
df_nSites <- data.table(get_df_nSites(nfi_df, nSites))

# Add groupID to climate
climate_df[,groupID := .GRP, by = siteID]

# Filter out unnecessary columns
tair_df <- climate_df[, c("time","tair","groupID")]

# Add year column
tair_df$year <- format(tair_df$time, format="%Y")

# Annual means for each groupID
tair_means_df <- data.table(aggregate(formula = (tair ~ groupID + year), data =  tair_df, FUN = mean))

# Mix type column to use (either CORINE forest class or mixture type)
join_col <- "mixtype"
join_df <- unique(df_nSites[, c("groupID", ..join_col)])

# Join forest classes
joined_tair_means_df <- left_join(tair_means_df, join_df, by = "groupID")

# Group as factor
joined_tair_means_df$groupID <- as.factor(joined_tair_means_df$groupID)

# Filter
filtered <- joined_tair_means_df

# Remove outliers:
# Replace upper outliers with max value from data with outliers removed
# Replace lower outliers with min value from data with outliers removed
outliers_removed <- filtered %>%
  group_by(groupID) %>%
  mutate(upper=get_upperQ(cur_data(),"tair"),lower=get_lowerQ(cur_data(),"tair")) %>%
  mutate(outlier = tair<lower | tair>upper) %>%
  mutate(tair = ifelse((outlier==T & tair<lower), min(filter(cur_data(), outlier==F)$tair), tair)) %>%
  mutate(tair = ifelse((outlier==T & tair>upper), max(filter(cur_data(), outlier==F)$tair), tair)) %>%
  ungroup() %>%
  reframe(.,.[,(1:4)]) %>%
  as.data.table(.)


# Plot
ggplot(data = outliers_removed, mapping = aes(x = year, y = tair, group=groupID, fill=groupID)) +
  geom_boxplot()

# Get mat ranges
max_mat <- joined_tair_means_df[, list(max_mat = max(tair)), by =  joined_tair_means_df[, ..join_col]]
min_mat <- joined_tair_means_df[, list(min_mat = min(tair)), by =  joined_tair_means_df[, ..join_col]]
mat <- left_join(max_mat, min_mat, by = join_col)

# Join with nfi
nfi_mat <- left_join(nfi_df, mat, by = join_col)












