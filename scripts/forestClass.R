# Script to determine CORINE forest classes for NFI plots as described in
# section 3.1.2 of Initial forest states document.


source('scripts/settings.R')
source('./r/utils.R')


# Get CORINE forest classes for nfi df based on species proportion of basal area in each site


path <- paste0(config$PATH_nfi_sweden,"sorted_group_species_cIDs_speciesID11to4_basal_area_lonLats.csv")
nfi_df <- fread(path)

# Get species codes df
species_codes_path <- paste0(config$PATH_nfi_sweden, "species_codes_with_logical_forest_type.csv")
species_codes_df <- fread(species_codes_path)

# Column name in nfi df with speciesID to be used
useSpeciesID <- "Species_code_name"

# Column name in species codes df with speciesID to be used
useSpeciesCode <- "code"

# Get as vector
ids_in_nfi <- unique(nfi_df[, get(useSpeciesID)])
ids_in_codes <- species_codes_df[, get(useSpeciesCode)]

# Filter codes
filtered_codes <- species_codes_df %>% filter(ids_in_codes %in% ids_in_nfi)

# Define BLT species ids
blt_ids <- filtered_codes[isBlt==T][,get(useSpeciesCode)]

# Define CON species ids
con_ids <- filtered_codes[isCon==T][,get(useSpeciesCode)]



# Site total basal area
nfi_df[,BAtot := sum(basal_area),by=groupID]

# Site tree basal area share
nfi_df[,ba_share := basal_area/BAtot]


# nfi_df[groupID==2]

# Get shares of tree species in site
shares <- nfi_df %>%
  group_by(groupID) %>%
  summarise(conif_share=sum(ba_share[get(useSpeciesID) %in% con_ids]),
            broadLeaf_share=sum(ba_share[get(useSpeciesID) %in% blt_ids])) %>%
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





