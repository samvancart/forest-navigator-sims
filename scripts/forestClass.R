source('scripts/settings.R')
source('./r/utils.R')


# Get CORINE forest classes for nfi df based on species proportion of basal area in each site


path <- paste0(nfi_sweden_path,"sorted_group_species_cIDs_speciesID11to4_basal_area_lonLats.csv")
nfi_df <- fread(path)

# Make deep copy
# temp_df <- nfi_df %>% as.data.table()


# Load speciesNames
speciesnames <- colnames(pCROB)
unique_speciesIDs <- unique(nfi_df$speciesID)
speciesnames[unique_speciesIDs]
coniferousIDs <- c(1,2)
broadLeavedIDs <- c(3,4,8)


# Site total basal area
nfi_df[,BAtot := sum(basal_area),by=groupID]

# Site tree basal area share
nfi_df[,ba_share := basal_area/BAtot]


# Get shares of tree species in site
shares <- nfi_df %>%
  group_by(groupID) %>%
  summarise(conif_share=sum(ba_share[speciesID %in% coniferousIDs]),
            broadLeaf_share=sum(ba_share[speciesID %in% broadLeavedIDs])) %>%
  ungroup()


# Assign forest class based on CORINE protocol
forest_class <- shares %>%
  group_by(groupID) %>%
  summarise(forest_class_name = get_forest_class_name(cur_data(),conif_share,broadLeaf_share)) %>%
  ungroup() %>%
  as.data.table()


# Forest class name column to df
nfi_df_forest_classes <- left_join(nfi_df,forest_class,by="groupID")

# Forest class ID column to df
setDT(nfi_df_forest_classes)[, forestClassID := .GRP, by = forest_class_name]


# Test
groupIDs <- unique(nfi_df$groupID)
check_mix_types <- sapply(groupIDs,function(x) setequal(forest_class[groupID == x]$mix_type,unique(nfi_df_forest_classes[groupID==x]$mix_type)))
setequal(length(unique(check_mix_types)),1)






