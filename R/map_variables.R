#' Return variables by matching variables.data
#'
#' @param variables.data named vector or list of variables data to match
#' @param match.to column provided variables.data should be matched to
#' @param dataset dataset
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
map_variables <- function(variables.data,
                          match.to,
                          dataset) {

  # check dataset
  dataset <- get_dataset(dataset)

  # No variables specified
  if (!hasArg(variables.data)) {
    return(.datasets[[dataset]][["variables"]] %>%
             dplyr::pull(var = "variables", name = NULL))
  }

  # Check match.to argument
  if (!hasArg(match.to)) {
    stop("Guessing not implemeted yet. Please provide an argument.")
  }


  # Check if input is vector
  vector.input <- tryCatch(is.atomic(variables.data),
                           error = function(cond) FALSE)

  # Check if input is vector
  list.input <- tryCatch(is.list(variables.data),
                         error = function(cond) FALSE)


  # if variables input expression
  if (!vector.input && !list.input) {
    stop(
    "This function only accepts a vector or a list for variable specification.
         Did you mean to use get_variables?")

  # ---- Match input to variables data
  } else if (vector.input || list.input) {

    variables.data <- as.list(variables.data)

    # Get variables data to match to (second dataset)
    var2data <- get_variables_data(variables = All,
                                   which = match.to,
                                   output.type = "list",
                                   dataset = dataset)


    variables.data.output <- lapply(
      X = variables.data,
      FUN = function(x) names(var2data)[match_v2l(x, var2data)])

    return(variables.data.output)


    # variables.data.output <- lapply(
    #     X = variables.data,
    #     FUN = function(x) names(var2data)[match(x, var2data)]) %>%
    #     lapply(function(x)
    #       paste(x, collapse = get_dataset_attr(which = "sep",
    #                                            dataset = dataset))) %>%
    #     unlist()


  }  else {

    stop("Incorrect input, but it's not clear what.
         Please either provide a list or a vector of variables data.")

  }

}


#' Return variables by matching variables.data
#'
#' @param variables.data named vector or list of variables data to match
#' @param which which variables data to pull
#' @param match.to column provided variables.data should be matched to
#' @param dataset dataset
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
map_variables_data <- function(variables.data,
                               which,
                               match.to,
                               dataset) {

  # check dataset
  dataset <- get_dataset(dataset)

  # No variables specified
  if (!hasArg(variables.data)) {
    return(.datasets[[dataset]][["variables"]] %>%
             dplyr::pull(var = "variables", name = NULL))
  }

  # Check match.to argument
  if (!hasArg(match.to)) {
    stop("Guessing not implemeted yet. Please provide an argument for matchto.")
  }

  vector.input <- tryCatch(is.atomic(variables.data),
                           error = function(cond) FALSE)

  # Check if input is vector
  list.input <- tryCatch(is.list(variables.data),
                         error = function(cond) FALSE)


  # if variables input expression
  if (!vector.input && !list.input) {
    stop(
      "This function only accepts a vector or a list for variable specification.
         Did you mean to use get_variables?")

    # input given as vector
    # intersect given proteins with proteins in dataset
  } else if (vector.input || list.input) {
    # Transform list to character
    variables.data <- as.list(variables.data)

    #
    var2data.from <- get_variables_data(variables = All,
                                        which = match.to,
                                        dataset = dataset)

    var2data.to <- get_variables_data(variables = All,
                                      which = which,
                                      dataset = dataset)


    variables.data.output <- variables.data %>%
      lapply(
        FUN = function(x)
          names(var2data.from)[match_v2l(x, var2data.from)]) %>%
      lapply(
        FUN = function(x)
          unique(var2data.to[x]))

    return(variables.data.output)


  } else {

    stop("Incorrect input, but it's not clear what.
         Please either provide a list or a vector of variables data.")

  }


}
