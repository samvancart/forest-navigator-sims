source('scripts/settings.R')
source('./r/mixTypeRules.R')


# Get nfi data
path <- paste0(nfi_sweden_path,"forest_classes.csv")
nfi_df <- fread(path)

# Filter
shares <- nfi_df[,c("groupID","speciesID","ba_share")]
shares <- shares[,sum(ba_share),by=c("groupID", "speciesID")]

# To percent
shares$V1 <- shares$V1*100

# To long format
shares_long = melt(shares,id.vars = c("groupID","speciesID"),measure.vars = c("V1"))
shares_long <- shares_long %>% select(-variable)

# Long to wide 
shares_wide <- reshape(shares_long, idvar = c("groupID"),
                     timevar = "speciesID", direction = "wide")

# To data table
shares_wide <- data.table(shares_wide)

# NAs to zero
shares_wide[is.na(shares_wide)] <- 0

# Share of soft woods
shares_wide$SOFT <- shares_wide$value.3 + shares_wide$value.8

# Drop unnecessary columuns
shares_wide <- shares_wide %>% select(c(-"value.3",-"value.8"))

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


nfi_df_mixTypes[groupID==2]
shares_wide[mixtype=="PS_PA"]
shares_wide[groupID==1]

# NAs should be zero
setequal(sum(is.na(shares_wide$mixtype)),0)




