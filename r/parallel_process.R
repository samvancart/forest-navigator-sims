
#' Execute a function in parallel across a list of items
#'
#' @description This function utilizes parallel processing to execute a given function (`FUN`) across a list of data frames (`data`) using the specified number of cores.
#'
#' @param data A list of items to be processed in parallel.
#' @param FUN A function to be applied to each item in the list.
#' @param FUN_args A list of additional arguments to be passed to `FUN`.
#' @param df_name An optional character name for `data` when passing the it to `FUN` with. If NULL `data` is passed as the first argument.
#' @param cores An integer specifying the number of cores to use for parallel processing. Defaults to 1.
#' @param type A character string specifying the cluster type. Defaults to "PSOCK".
#' @param ... Additional arguments to be passed to `foreach`.
#'
#' @return A list containing the results of applying `FUN` to each item in `data`.
#'
#' @import parallel
#' @import foreach
#' @import doParallel
#' @import checkmate
#' @export
#' @examples
#' data_list <- list(data.frame(a = 1:10), data.frame(b = 11:20))
#' result <- get_in_parallel(data_list, function(df) summary(df), list(), cores = 2)
#' print(result)
get_in_parallel <- function(data, FUN, FUN_args, df_name = NULL, cores = 1, type = "PSOCK", ...) {
  # Input validations
  # checkmate::assert_list(data, min.len = 1, types = "data.frame")
  checkmate::assert_function(FUN)
  checkmate::assert_list(FUN_args, names = "named")
  checkmate::assert_character(df_name, len = 1, null.ok = TRUE)
  checkmate::assert_int(cores, lower = 1)
  checkmate::assert_choice(type, choices = c("PSOCK", "FORK"))
  
  tryCatch(
    {
      cat("Processing on" , cores, "core(s)...", "\n")
      cat("Type:", type, "\n")
      
      cl <- parallel::makeCluster(cores, type = type, outfile = "") # Make cluster
      on.exit(parallel::stopCluster(cl))  # Stop cluster on exit
      registerDoParallel(cl)
      
      t <- system.time(
        result <- foreach(df = data, ...) %dopar% {
          args <- if (is.null(df_name)) {
            c(list(df), FUN_args)
          } else {
            c(setNames(list(df), df_name), FUN_args)
          }
          do.call(FUN, args) # Call function with constructed arguments
        }
      )
      cat(t)
      return(result)
    },
    error = function(e) {
      stop(e)
    }
  )
}

