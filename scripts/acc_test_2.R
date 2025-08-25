

library(optparse)
library(checkmate)

# PARSE_ARGS --------------------------------------------------------------

option_list <- list(
  make_option(c("-c", "--countries"), type = "character", default = NA,
              help = "Country names or abbreviations (e.g., 'FI' or 'Finland' or multiple e.g., 'se,FI' or 'Sweden, finland')"),
  make_option(c("-o", "--output_type"), type = "character", help = "output file type either output_files or dbh_classes.")
)

parser <- OptionParser(option_list = option_list)
args <- parse_args(parser)

assert_true(c("output_type") %in% names(args))

countries_arg <- args$countries
countries <- if (is.na(countries_arg)) NA else strsplit(countries_arg, ",")[[1]]
countries <- trimws(countries)  # Remove spaces around items


print(args)
print(countries)
print(args$output_type)











