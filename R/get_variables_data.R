#' Return variables data
#'
#' @param variables (optional) vector of variables or expression
#' @param which which variables data to pull (multiple supported)
#' @param output.type output type (default = "vector" or "tibble_inlist" for
#' multiple arguments; combination of "vector", "list", "tibble", or
#' "data.frame" to define data type and "_inlist" or "" if data should be put
#' into a list or not)
#' @param FUN function to apply to variables data (examples are strsplit,
#'  keep_first, keep_firstn)
#' @param ... arguments for FUN function
#' @param dataset dataset
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
get_variables_data <- function(variables,
                               which,
                               output.type = "vector",
                               FUN,
                               ...,
                               dataset) {

  # check dataset
  dataset <- get_dataset(dataset = dataset)


  ### Variables

  # No variables defined
  if (!hasArg(variables)) {

    variables <- get_variables(dataset = dataset)

  # Variables defined
  } else {

    # Check if input is vector
    vector.input <- tryCatch(is.vector(variables),
                             error = function(cond) FALSE)

    # Default variables
    if (vector.input && length(variables) == 1 && variables == "default") {
      variables <- get_variables(variables = "default", dataset = dataset)
    }


    # if variables input is vector
    if (!vector.input) {
      variables <- get_variables(variables = !!dplyr::enquo(variables),
                                 dataset = dataset)
    }

  }


  ### Which data to pull

  # No argument given (which)
  if (!hasArg(which)) {
    which <- select.list(
      choices = get_variables_data_names(dataset = dataset),
      multiple = TRUE,
      title = "Select variables data columns:")

    # Test which
    if (length(which) == 0) {
      return(NULL)
    }

  }


  # Check if names are all in variables data
  if (any(!which %in% get_variables_data_names(dataset = dataset))) {

    message(paste0("Some data names were not found: \n  ",
                   paste(which[!which %in%
                                 get_variables_data_names(dataset = dataset)],
                         collapse = "\n  ")))

    # Transfer variables data if possible
    if (any(which[!which %in%
                  get_variables_data_names(dataset = dataset)] %in%
            available_variables_data(dataset = dataset,
                                     view = F,
                                     return = T,
                                     print.call = F))) {

      # Message which variables data is transferred
      message(paste0(
      "\nFollowing names were found in raw data and will be transferred: ",
      "\n  ",
      paste(which[which %in% available_variables_data(dataset = dataset,
                                                      view = F,
                                                      return = T,
                                                      print.call = F) &
                    !which %in% get_variables_data_names(dataset = dataset)],
                           collapse = "\n  ")))

      # Transfer missing variables data
      transfer_variables_data(
        dataset = dataset,
        data.columns = which[which %in%
                               available_variables_data(dataset = dataset,
                                                        view = F,
                                                        return = T,
                                                        print.call = F) &
                               !which %in%
                               get_variables_data_names(dataset = dataset)])


    }

  }



  # Adjust default for multiple variables data
  if (length(which) > 1 && grepl(pattern = "vector", x = output.type)) {
    output.type <- "tibble_inlist"
  }

  # Check modify function
  if (hasArg(FUN) && !is.function(FUN)) {
    message("No function provided to FUN. Output will not be modified.")
    FUN <- function(x) {x}
  }


  # One variables data required
  if (length(which) == 1) {

    if (grepl(pattern = "vector", x = output.type)) {

      data <- .datasets[[dataset]][["variables"]] %>%
        dplyr::filter(.data[["variables"]] %in% !!variables) %>%
        dplyr::arrange(match(.data[["variables"]], !!variables)) %>%
        dplyr::pull(var = !!dplyr::enquo(which), name = "variables")

      # Modify
      if (hasArg(FUN)) {
        data.mod <- FUN(data)
        if (length(data) == length(data.mod) && is.vector(data.mod)) {
          data <- data.mod
        } else {
          message(paste0("Modifying function would change the length or type ",
          "of the output. Vector not modified. Consider list output."))
        }
      }

    # List
    } else if (regexpr(pattern = "list", text = output.type) == 1) {

      data <- .datasets[[dataset]][["variables"]] %>%
        dplyr::filter(.data[["variables"]] %in% !!variables) %>%
        dplyr::arrange(match(.data[["variables"]], !!variables)) %>%
        dplyr::pull(var = !!dplyr::enquo(which), name = "variables") %>%
        as.list()

      # Modify
      if (hasArg(FUN)) {
        data.mod <- FUN(data, ...)
        if (length(data) == length(data.mod) && is.list(data.mod)) {
          data <- data.mod
        } else {
          message(paste0("Modifying function would change the length or type ",
          "of the output. List not modified. Consider list output."))
        }
      }

    }

  # Multiple variables data names
  } else if (grepl(pattern = "tibble", x = output.type)) {

    data <- .datasets[[dataset]][["variables"]] %>%
      dplyr::filter(.data[["variables"]] %in% !!variables) %>%
      dplyr::arrange(match(.data[["variables"]], !!variables)) %>%
      dplyr::select("variables", !!which)

    # Modify
    if (hasArg(FUN)) {
      data <- lapply(data, FUN, ...)
    }

  } else if (grepl(pattern = "data.frame", x = output.type)) {

    data <- .datasets[[dataset]][["variables"]] %>%
      dplyr::filter(.data[["variables"]] %in% !!variables) %>%
      dplyr::arrange(match(.data[["variables"]], !!variables)) %>%
      dplyr::select("variables", !!which) %>%
      tibble2data.frame(row.names = "variables")

    # Modify
    if (hasArg(FUN)) {
      data <- lapply(data, FUN, ...)
    }

  # Output type not found
  } else {

    message(paste0("Output type <",
                   output.type,
                   "> not supported. Use <vector> and <list> for single ",
                   "variables data calls or <tibble> and <data.frame> for ",
                   "single and multiple variables data calls with the ",
                   "optional suffix <_inlist>."))

    return(invisible(NULL))

  }



  # Put data in list
  if (grepl(pattern = "_inlist", x = output.type)) {

    if (!hasArg(output)) output <- ifelse(length(which) == 1,
                                          which,
                                          "variables_data")

    # Add data to list
    data_ <- tibble::lst(
      !!output := data)

  } else {
    data_ <- data
  }


  # Return data
  return(data_)

}

#' Return variables data
#'
#' @param variables (optional) vector of variables or expression
#' @param which which variables data to pull (multiple supported)
#' @param output.type output type (default = "vector" or "tibble_inlist" for
#' multiple arguments; combination of "vector", "list", "tibble", or
#' "data.frame" to define data type and "_inlist" or "" if data should be put
#' into a list or not)
#' @param FUN function to apply to variables data (examples are strsplit,
#'  keep_first, keep_firstn)
#' @param ... arguments for FUN function
#' @param dataset dataset
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
get_var_data <- get_variables_data

