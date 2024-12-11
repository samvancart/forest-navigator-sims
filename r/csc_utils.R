
# Functions



#' Cast a value to a specified type
#'
#' This function takes a value and a type, and casts the value to the specified type.
#' Supported types are "integer", "character", and "logical".
#' If an unsupported type is provided, the function will stop with an error message.
#'
#' @param value The value to be cast.
#' @param type The type to cast the value to.
#' @return The value cast to the specified type.
#' @examples
#' cast_value("42", "integer") # returns 42 as integer
#' cast_value("TRUE", "logical") # returns TRUE as logical
#' @export
cast_value <- function(value, type) {
  switch(type,
         "integer" = as.integer(value),
         "character" = as.character(value),
         "logical" = as.logical(value),
         stop("Unsupported type provided")
  )
}

#' Retrieve a system environment parameter with a default
#'
#' This function retrieves a system environment variable by name.
#' If the variable is not set or is empty, a default value is returned, cast to a specified type.
#' The function uses 'cast_value' to perform the type casting.
#'
#' @param param_name The name of the system environment variable to retrieve.
#' @param default The default value to return if the variable is not set or is empty.
#' @param type The type to cast the returned value to.
#' @return The value of the system environment variable or the default value, cast to the specified type.
#' @examples
#' get_parameter("MY_VAR", "default_value", "character") # returns the value of MY_VAR or "default_value"
#' @export
get_parameter <- function(param_name, default, type) {
  param <- Sys.getenv(param_name)
  value <- ifelse(is.na(param) | param=="", cast_value(default, type), cast_value(param, type))
  return(value)
}




















