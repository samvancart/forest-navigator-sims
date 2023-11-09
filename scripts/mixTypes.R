source('scripts/settings.R')
source('./r/mixTypeRules.R')
source('./r/utils.R')


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

# Define SOFT species ids
soft_ids <- c(filtered_codes[shortName=="SOFT"][,c(..useSpeciesCode)])[[1]]


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




# Get SOFT columns
soft_cols <- get_colnames_with_prefix_from_ids(soft_ids)

# Share of soft woods
shares_wide$SOFT <- rowSums(shares_wide[, ..soft_cols])

# Drop unnecessary columuns
shares_wide <- shares_wide %>% select(-all_of(soft_cols))

# Rename columns
colnames(shares_wide) <- c("groupID","PS","PA","FS","SOFT")


# Share of broad leaved trees
shares_wide$BLT <- shares_wide$FS + shares_wide$SOFT

# Share of conifers
shares_wide$CON <- shares_wide$PS + shares_wide$PA

# Get rid of possible rounding errors in mixtype function
shares_wide <- round(shares_wide,7)


# All required columns
#"PA", "FS", "QU", "PS", "PN", "DF", "LD", "AA", "PC", "CB", "SOFT", "AC", "BLT", "CON"

# Add rest of required columns
shares_wide[,c("QU", "PN", "DF", "LD", "AA", "PC", "CB", "AC")] <- 0

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


