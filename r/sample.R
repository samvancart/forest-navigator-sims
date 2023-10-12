
# Functions

# Generate samples from data
# Params:
# original (integer vector): original data
# new (integer vector): data to generate
# Returns:
# data.table: A table with columns k and v 
# k: All the data (original + new) representing keys
# v: Generated data representing values

get_samples_df <- function(original, new, seed=1234, replace=T) {
  # Define variables
  all <- c(original, new)
  nSample <- length(new) ## Number of values that need to be sampled
  
  # Create table of keys and values
  df <- data.table(k = all, v = all)
  df <- df[k %in% new, v:=NA] ## Set new v to NA
  
  set.seed(seed)
  
  # Create sample data and fill df
  sampleX <- sample(x=original, size=nSample, replace = replace)
  df <- df[k %in% new, v:=sampleX] ## Fill the table
  
  
  return(df)
}

# Get leap years from vector of years
get_leap_years <- function(years) {
  return(years[leap_year(years)])
}

# Get non leap years from vector of years
get_non_leap_years <- function(years) {
  return(years[!leap_year(years)])
}

# Helper to change the years in a vector of dates
get_new_dates <- function(time, year) {
  d <- day(time)  
  m <- month(time)
  y <- year
  dates <- paste0(as.character(d),"/",as.character(m),"/",as.character(y))
  new_time <- dmy(dates)
  
  return(new_time)
   
}

# Df for one specific year in original data
get_one_year_data <- function(x, df) {
  one_year<-filter(df, year(time) == x[2])
  one_year$time <- get_new_dates(one_year$time, x[1])
  
  return(one_year)
  
}

# Check that old and new dataframes have matching data
check_equal <- function(x,df_all,df,cols) {
  new <- filter(df_all,year(time)==x[1])[, cols]
  old <- filter(df,year(time)==x[2])[, cols]
  check <- setequal(new,old)
  return(check)
}





