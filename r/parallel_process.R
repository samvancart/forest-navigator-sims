

#' Execute a function in parallel across a list of items
#'
#' @description This function utilizes parallel processing to execute a given function (`FUN`) across a list of data frames (`data`) using the specified number of cores.
#'
#' @param data A list of items to be processed in parallel.
#' #' @param FUN A function to be applied to each item in the list.
#' #' @param FUN_args A list of additional arguments to be passed to `FUN`.
#' #' @param df_name An optional character name for `data` when it is not the first argument of `FUN`. If NULL `data` is passed as the first argument.
#' #' @param cores An integer specifying the number of cores to use for parallel processing. Defaults to 1.
#' #' @param type A character string specifying the cluster type. Defaults to "PSOCK".
#' #' @param ... Additional arguments to be passed to `foreach`.
#' #'
#' #' @return A list containing the results of applying `FUN` to each item in `data`.
#' #'
#' #' @import parallel
#' #' @import foreach
#' #' @import doParallel
#' #' @import checkmate
#' #' @export
#' #' @examples
#' #' data_list <- list(data.frame(a = 1:10), data.frame(b = 11:20))
#' #' result <- get_in_parallel(data_list, function(df) summary(df), list(), cores = 2)
#' #' print(result)
#' get_in_parallel <- function(data, FUN, FUN_args, df_name = NULL, cores = 1, type = "PSOCK", ...) {
#'   # Input validations
#'   # checkmate::assert_list(data, min.len = 1, types = "data.frame")
#'   checkmate::assert_function(FUN)
#'   checkmate::assert_list(FUN_args, names = "named")
#'   checkmate::assert_character(df_name, len = 1, null.ok = TRUE)
#'   checkmate::assert_int(cores, lower = 1)
#'   checkmate::assert_choice(type, choices = c("PSOCK", "FORK"))
#'   
#'   
#'   tryCatch(
#'     {
#'       cat("Processing on" , cores, "core(s)...", "\n")
#'       cat("Type:", type, "\n")
#'       
#'       cl <- parallel::makeCluster(cores, type = type, outfile = "") # Make cluster
#'       on.exit(parallel::stopCluster(cl))  # Stop cluster on exit
#'       registerDoParallel(cl)
#'       
#'       # Define args_FUN outside foreach
#'       args_FUN <- get_parallel_args
#'       
#'       t <- system.time(
#'         result <- foreach(df = data, ...) %dopar% {
#'           # Check if df_name should be used
#'           args <- do.call(args_FUN, list(df = df, FUN_args = FUN_args, df_name = df_name))
#'           
#'           do.call(FUN, args) # Call function with constructed arguments
#'         }
#'       )
#'       cat(t)
#'       return(result)
#'     },
#'     error = function(e) {
#'       stop(e)
#'     }
#'   )
#' }




# get_in_parallel <- function(data, FUN, FUN_args, df_name = NULL, cores = 1, type = "PSOCK") {
#   assert_choice(type, choices = c("PSOCK", "FORK"))
#   
#   tryCatch({
#     cat("Processing on", cores, "core(s)...\n")
#     cat("Type:", type, "\n")
#     
#     # Set future plan based on type
#     if (type == "FORK") {
#       plan(multicore, workers = cores)  # Unix/macOS only
#     } else {
#       plan(multisession, workers = cores)  # Cross-platform
#     }
#     
#     args_FUN <- get_parallel_args
#     
#     t <- system.time({
#       result <- future_map(data, function(df) {
#         args <- args_FUN(df = df, FUN_args = FUN_args, df_name = df_name)
#         do.call(FUN, args)
#       })
#     })
#     
#     cat(t)
#     return(result)
#     
#   }, error = function(e) {
#     message("Error during parallel execution: ", e$message)
#     stop(e)
#   })
# }


#' Execute a function in parallel across a list of items
#'
#' @description This function utilizes parallel processing to execute a given function (`FUN`) across a list of data frames (`data`) using the specified number of cores.
#'
#' @param data A list of items to be processed in parallel.
#' @param FUN A function to be applied to each item in the list.
#' @param FUN_args A list of additional arguments to be passed to `FUN`.
#' @param df_name An optional character name for `data` when it is not the first argument of `FUN`. If NULL `data` is passed as the first argument.
#' @param cores An integer specifying the number of cores to use for parallel processing. Defaults to 1.
#' @param type A character string specifying the cluster type. Either "PSOCK" or "FORK".
#' @param .options A `furrr_options()` object specifying future-specific options like packages and globals.
#' @param .env_globals Environment to look for globals required by `.x` and `...`.
#' @param .progress Logical. Should a progress bar be displayed?
#' @param .id Either a string or NULL. If a string, the output will contain a variable with that name.
#' @param ... Additional arguments passed to the mapped function.
#'
#' @return A list containing the results of applying `FUN` to each item in `data`.
#' @export
get_in_parallel <- function(
    data,
    FUN,
    FUN_args,
    df_name = NULL,
    cores = 1,
    type = "PSOCK",
    .options = furrr_options(),
    .env_globals = NULL,
    .progress = FALSE,
    .id = NULL,
    ...
) {
  # Input validations
  # assert_list(data, min.len = 1, types = "data.frame")
  assert_function(FUN)
  assert_list(FUN_args, names = "named")
  assert_character(df_name, len = 1, null.ok = TRUE)
  assert_int(cores, lower = 1)
  assert_choice(type, choices = c("PSOCK", "FORK"))
  
  tryCatch({
    cat("Processing on", cores, "core(s)...\n")
    cat("Type:", type, "\n")
    
    # Set future plan
    if (type == "FORK") {
      plan(multicore, workers = cores)
    } else {
      plan(multisession, workers = cores)
    }
    
    args_FUN <- get_parallel_args
    
    t <- system.time({
      result <- future_map(
        .x = data,
        .f = function(df, ...) {
          args <- args_FUN(df = df, FUN_args = FUN_args, df_name = df_name)
          do.call(FUN, args)
        },
        .options = .options,
        .env_globals = .env_globals,
        .progress = .progress,
        .id = .id,
        ...
      )
    })
    
    cat(t)
    return(result)
    
  }, error = function(e) {
    message("Error during parallel execution: ", e$message)
    stop(e)
  })
}


# Get args from df_name if not NULL
get_parallel_args <- function(df, FUN_args, df_name = NULL) {
  
  args <- if (is.null(df_name)) {
    c(list(df), FUN_args)
  } else {
    c(setNames(list(df), df_name), FUN_args)
  }
  
  return(args)
}



