
# Forest Navigtor mixture type rules function
# Source: gitlab folder 'wp3/Simulation_protocol'



if(!require(dplyr)) { install.packages("dplyr"); require(dplyr)}

#necessary and used species columns
#AA	abies alba	silver fir
#AC	acer	maple includes following acer species: acer campestre, acer platanoides, acer pseudoplatanus
#BLT	broad leaf tree	
#CB	carpinus betulus	hornbeam
#CON	conifers	
#DF	douglas fir	
#FS	fagus sylvatica	beech
#LD	larix decidua	european larch
#PA	picea abies	norway spruce
#PN	pinus nigra	black pine
#PS	pinus sylvestris	scots pine
#QU	quercus	oak includes following quercus species: quercus petraea, quercus pubescens, quercus robur, quercus rubra
#SOFT	soft woods	includes following broad leaf tree species: alnus glutinosa, alnus incana, alnus viridis, betula pendula, populus nigra, populus tremula

#Input is a data.frame with following columns (more are possible but not used)
#"PA", "FS", "QU", "PS", "PN", "DF", "LD", "AA", "PC", "CB", "SOFT", "AC", "BLT", "CON"

#-------------------- mix type assignment from ForestNavigator ------------------------------
AssignMixtypesForestNavigator <- function(SpeciesData) {
  SpeciesData$mixtype <- NA
  #----- norway spruce types
  SpeciesData <- SpeciesData %>% 
    mutate(mixtype_temp = case_when(
      PA>=90 & BLT==0 ~ "PA3",
      PA>=90 & BLT!=0 ~ "PA1",
      PA>80 & LD>=10 ~ "PA2",
      PA>=80 ~ "PA4",
      PA>50 & FS>=20 & PA+FS>=90 ~ "PA_FS",
      PA>50 & LD>=20 & PA+LD>=90 ~ "PA_LD",
      PA>50 & PS>=20 & PA+PS>=90 ~ "PA_PS",
      PA>50 & AA>=20 & PA+AA>=90 ~ "PA_AA",
      PA>50 & CON-PA>=20 & CON>=90 ~ "PA_CON",
      PA>50 & BLT>=20 & PA+BLT>=90 ~ "PA_BLT",
      PA>50 ~ "PA_MIX"), mixtype=ifelse(is.na(mixtype_temp), mixtype, mixtype_temp))
  
  #----- beech types
  SpeciesData <- SpeciesData %>% 
    mutate(mixtype_temp = case_when(
      FS>=90  ~ "FS",
      FS>=50 & PA>=20 & FS+PA>=90 ~ "FS_PA",
      FS>50 & BLT-FS>=20 & BLT>=90  ~ "FS_BLT",
      FS>=50 & AA>=10 & PA>=10 & (FS+PA+AA>=80) ~ "FS_PA_AA",
      FS>50 & CON>=20 & FS+CON>=90   ~ "FS_CON",
      FS>50 ~ "FS_MIX"
    ), mixtype=ifelse(is.na(mixtype_temp), mixtype, mixtype_temp))
  
  #----- oak types
  SpeciesData <- SpeciesData %>% 
    mutate(mixtype_temp = case_when(
      QU>=90  ~ "QU",
      QU>=50 & FS>20 & QU+FS>=90 ~ "QU_FS",
      QU>50 & PS>20 & QU+PS>=90 ~ "QU_PS",
      QU>50 & BLT-QU>=20 & BLT>=90 ~ "QU_BLT",
      QU>50  ~ "QU_MIX"
    ), mixtype=ifelse(is.na(mixtype_temp), mixtype, mixtype_temp))
  
  #----- pine types (scots and black)
  SpeciesData <- SpeciesData %>% 
    mutate(mixtype_temp = case_when(
      PS>=90  ~ "PS",
      PS>=50 & PA>20 & PS+PA>=90 ~ "PS_PA",
      PS>=50 & FS>20 & PS+FS>=90 ~ "PS_FS",
      PS>=50 & QU>20 & PS+QU>=90 ~ "PS_QU",
      PS>50 & CON-PS>=20 & CON>=90  ~ "PS_CON",
      PS>=50 & BLT>=20 & PS+BLT>=90  ~ "PS_BLT",
      PS>50  ~ "PS_MIX",
      PN>=90  ~ "PN",
      PN>50 ~ "PN_MIX"
    ), mixtype=ifelse(is.na(mixtype_temp), mixtype, mixtype_temp))
  
  #----- european larch types
  SpeciesData <- SpeciesData %>% 
    mutate(mixtype_temp = case_when(
      LD>=90  ~ "LD",
      LD>=50 & PA>20 & LD+PA>=90 ~ "LD_PA",
      LD>=50 & FS>20 & LD+FS>=90 ~ "LD_FS",
      LD>=50 & QU>20 & LD+QU>=90 ~ "LD_QU",
      LD>=50 & CON-LD>=20 & CON>=90 ~ "LD_CON",
      LD>=50 ~ "LD_MIX"
    ), mixtype=ifelse(is.na(mixtype_temp), mixtype, mixtype_temp))
  
  #----- silver fir types
  SpeciesData <- SpeciesData %>% 
    mutate(mixtype_temp = case_when(
      AA>=90  ~ "AA",
      AA>=50 & PA>20 & AA+PA>=90 ~ "AA_PA",
      AA>=50 & FS>20 & AA+FS>=90 ~ "AA_FS",
      AA>=50 & CON-AA>=20 & CON>=90 ~ "AA_CON",
      AA>=50 ~ "AA_MIX"
    ), mixtype=ifelse(is.na(mixtype_temp), mixtype, mixtype_temp))
  
  #----- douglas fir types
  SpeciesData <- SpeciesData %>% 
    mutate(mixtype_temp = case_when(
      DF>90  ~ "DF",
      DF>50 ~ "DF_MIX"
    ), mixtype=ifelse(is.na(mixtype_temp), mixtype, mixtype_temp))
  
  #----- special mix types
  SpeciesData <- SpeciesData %>% 
    mutate(mixtype_temp = case_when(
      !is.na(mixtype) ~ mixtype, # keep mix type if already assigned
      AA>10 & FS>10 & PA>10 & PA+FS+AA>80  ~ "PA_AA_FS2",
      FS>10 & PA>10 & PA+FS>70 & PA<=FS  ~ "FS_PA2",
      FS>10 & PA>10 & PA+FS>70 & PA>FS  ~ "PA_FS2"
    ), mixtype=ifelse(is.na(mixtype_temp), mixtype, mixtype_temp))
  
  #----- broad leaf types
  SpeciesData <- SpeciesData %>% 
    mutate(mixtype_temp = case_when(
      !is.na(mixtype) ~ mixtype, # keep mix type if already assigned
      BLT>90 & SOFT>50  ~ "BLT_SOFT",
      BLT>90 & FS>30 ~ "BLT_FS",
      BLT>90 & QU>30 ~ "BLT_QU",
      BLT>90 ~ "BLT"  ), mixtype=ifelse(is.na(mixtype_temp), mixtype, mixtype_temp))
  
  SpeciesData <- SpeciesData %>% 
    mutate(mixtype_temp = case_when(
      !is.na(mixtype) ~ mixtype, # keep mix type if already assigned
      BLT>50 & PA>30  ~ "BLT_PA",
      BLT>50 & FS+AC>50 ~ "BLT_FS_AC",
      BLT>50 & QU+CB>50 ~ "BLT_QU_CB",
      BLT>50 ~ "BLT_MIX"  ), mixtype=ifelse(is.na(mixtype_temp), mixtype, mixtype_temp))
  
  #----- conifer types
  SpeciesData <- SpeciesData %>% 
    mutate(mixtype_temp = case_when(
      !is.na(mixtype) ~ mixtype, 
      CON>90  ~ "CON",
      CON>=50 ~ "CON_MIX"), mixtype=ifelse(is.na(mixtype_temp), mixtype, mixtype_temp))
  
  #remove some of the not needed columns
  SpeciesData[, c("mixtype_temp")] <- NULL
  SpeciesData
}
