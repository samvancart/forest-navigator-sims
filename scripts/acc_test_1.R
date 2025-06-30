# R/write_params.R
library(optparse)

# PARSE_ARGS --------------------------------------------------------------

option_list <- list(
  make_option(c("-c", "--countries"), type = "character", default = NA,
              help = "Country names or abbreviations (e.g., 'FI' or 'Finland' or multiple e.g., 'se,FI' or 'Sweden, finland')")
)

parser <- OptionParser(option_list = option_list)
args <- parse_args(parser)

countries_arg <- args$countries
countries <- if (is.na(countries_arg)) NA else strsplit(countries_arg, ",")[[1]]
countries <- trimws(countries)  # Remove spaces around items

print(args)
print(countries)




