source('scripts/settings.R')
source('./r/mixTypeRules.R')
source('./r/utils.R')

# All required columns
#"PA", "FS", "QU", "PS", "PN", "DF", "LD", "AA", "PC", "CB", "SOFT", "AC", "BLT", "CON"



## REMEMBER TO USE SAME CODES FOR FOREST CLASSES!!! ###



# Get nfi data
path <- paste0(nfi_sweden_path,"forest_classes.csv")
nfi_df <- fread(path)

# Get species codes df
species_codes_path <- paste0(nfi_sweden_path, "speciesCodesSweden.csv")
species_codes_df <- fread(species_codes_path)

# Column name in nfi df with speciesID to be used
useSpeciesID <- "Species_code_name"

# Column name in species codes df with speciesID to be used
useSpeciesCode <- "code"

# Get as vector
ids_in_nfi <- c(unique(nfi_df[, c(..useSpeciesID)]))[[1]]
ids_in_codes <- c(species_codes_df[, ..useSpeciesCode])[[1]]

# Filter codes
filtered_codes <- species_codes_df %>% filter(ids_in_codes %in% ids_in_nfi)



# DEFINE VARS

# All required columns for mixtype function
required_cols <- c("PA", "FS", "QU", "PS", "PN", "DF", "LD", "AA", "PC", "CB", "SOFT", "AC", "BLT", "CON")

# Column of mix type names in species codes df
name_col <- "shortName"

# Define BLT species ids
blt_ids <- c(filtered_codes[isBlt==T][,c(..useSpeciesCode)])[[1]]

# Get BLT columns
blt_cols <- get_colnames_with_prefix_from_ids(blt_ids)

# Define CON species ids
con_ids <- c(filtered_codes[isCon==T][,c(..useSpeciesCode)])[[1]]

# Get CON columns
con_cols <- get_colnames_with_prefix_from_ids(con_ids)

# Get SOFT columns
soft_cols <- combine_grouped_mixtype_cols(filtered_codes,useSpeciesCode,name_col,short_name ="SOFT")

# Get AC columns
ac_cols <- combine_grouped_mixtype_cols(filtered_codes,useSpeciesCode,name_col,short_name ="AC")

# Get QU columns
qu_cols <- combine_grouped_mixtype_cols(filtered_codes,useSpeciesCode,name_col,short_name ="QU")

# Define CON and BLT cols to drop
drop_cols_ids <- c(filtered_codes[shortName=="CON" | shortName=="BLT"][, c(..useSpeciesCode)])[[1]]

# Get columns to drop
drop_cols <- get_colnames_with_prefix_from_ids(drop_cols_ids)




# BUILD WIDE DATA TABLE

# Filter
shares <- nfi_df[,c("groupID", ..useSpeciesID, "ba_share")]
shares <- shares[,sum(ba_share),by=c("groupID", useSpeciesID)]

# To percent
shares$V1 <- shares$V1*100

# To long format
shares_long = melt(shares, id.vars = c("groupID", useSpeciesID), measure.vars = c("V1"))
shares_long <- shares_long %>% select(-variable)

# Long to wide 
shares_wide <- reshape(shares_long, idvar = c("groupID"),
                     timevar = useSpeciesID, direction = "wide")

# To data table
shares_wide <- data.table(shares_wide)

# NAs to zero
shares_wide[is.na(shares_wide)] <- 0



# AGGREGATE

# Share of broad leaved trees
shares_wide$BLT <- rowSums(shares_wide[, ..blt_cols])


# Share of conifers
shares_wide$CON <- rowSums(shares_wide[, ..con_cols])


# Share of soft woods
shares_wide$SOFT <- rowSums(shares_wide[, ..soft_cols])

# Share of AC
shares_wide$AC <- rowSums(shares_wide[, ..ac_cols])

# Share of QU
shares_wide$QU <- rowSums(shares_wide[, ..qu_cols])


# Columns of combined species
combined_cols <- append(append(soft_cols,ac_cols),qu_cols)

# Drop unnecessary columuns
shares_wide <- shares_wide %>% select(-all_of(append(combined_cols,drop_cols)))

# Get rid of possible rounding errors in mixtype function
shares_wide <- round(shares_wide,7)

# Get all column names
all_cols <- unique(append(con_cols,blt_cols))

# Filter by dropping SOFTs
filtered_cols <- all_cols[which(!all_cols %in% combined_cols)]

# Columns to rename in data table
rename_cols <- filtered_cols[which(!filtered_cols %in% drop_cols)]

# Get new column names
names <- sapply(colnames(shares_wide), function (x) rename_column(x, useSpeciesCode = useSpeciesCode, name = name_col))

# Rename columns
colnames(shares_wide) <- names

# Get rest of required column names to be added
add_cols <- required_cols[!required_cols %in% names]

# Add rest of required columns
shares_wide[,add_cols] <- 0




# RUN FUNCTION

# Assign mix types
shares_wide <- AssignMixtypesForestNavigator(shares_wide)

# Filter columns
mix_types <- shares_wide[,c("groupID", "mixtype")]

# Add mixtype column to nfi df
nfi_df_mixTypes <- left_join(nfi_df, mix_types, by="groupID")

unique(nfi_df_mixTypes$mixtype)

nfi_df_mixTypes[groupID==2]
shares_wide[mixtype=="PS_PA"]
shares_wide[groupID==1]

# Number of NAs should be zero
setequal(sum(is.na(shares_wide$mixtype)),0)


