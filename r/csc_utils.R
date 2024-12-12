
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




#' Get Named Vector
#'
#' This function captures the names and values of the variables passed to it and returns them as a named vector.
#'
#' @param ... The variables you want to capture.
#'
#' @return A named vector with the names of the variables as the names of the vector elements and their corresponding values.
#'
#' @examples
#' var1 <- 10
#' var2 <- 20
#' var3 <- "apple"
#' get_named_vector(var1, var2, var3)
#'
#' @export
#'
#' @importFrom rlang enquo
#' @importFrom rlang enquos
#' @importFrom rlang sym
#' @importFrom rlang syms
get_named_list <- function(...) {
  # Capture the expressions passed to the function
  args <- list(...)
  
  if (length(args) == 0) {
    stop("No variables provided to get_named_vector function.")
  }
  
  # Use deparse and substitute to get the variable names
  names <- sapply(substitute(list(...)), deparse)
  names(args) <- names[2:length(names)]
  
  return(args)
}

#' Get Named Vector as Text
#'
#' This function takes a named vector and concatenates each name with its corresponding value, separated by a specified text.
#'
#' @param named_vec A named vector with elements to be concatenated.
#' @param sep_text A string to separate the name and the value of each element in the named vector.
#' @return A character vector where each element is a concatenation of the name and the value from the named vector.
#' @examples
#' named_vector <- c(a = 1, b = 2, c = 3)
#' get_named_vector_as_txt(named_vector, sep_text = ": ")
#' @export
#'
#' @importFrom stats setNames
get_named_list_as_txt <- function(named_vec, sep_text="") {
  
  if (length(named_vec) == 0) {
    stop("No variables provided to get_named_vector function.")
  }
  
  txt_vec <- invisible(unlist(lapply(seq_along(named_vec), function(x) {
    name <- names(named_vec)[x]
    var <- named_vec[x]
    sep_text <- sep_text
    text <- paste0(name, sep_text, var)
  })))
  
  return(txt_vec)
}















