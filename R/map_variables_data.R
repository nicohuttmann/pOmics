#' Return variables by matching variables.data
#'
#' @param variables.data named vector or list of variables data to match
#' @param which which variables data to pull
#' @param match.to column provided variables.data should be matched to
#' @param unique.matches remove redundant matches?
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
                                 unique.matches = T,
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
  vector.input <- tryCatch(is.vector(variables.data),
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
  } else if (vector.input) {
    # Transform list to character
    variables.data <- as.list(variables.data) %>%
      lapply(function(x) strsplit_(x,
                                   split = get_dataset_attr(which = "sep",
                                                            dataset = dataset)))

    #
    var2data.from <- get_variables_data(variables = All,
                                        which = match.to,
                                        dataset = dataset)

    var2data.to <- get_variables_data(variables = All,
                                      which = which,
                                      dataset = dataset)

    if (unique.matches) {
      variables.data.output <- variables.data %>%
        lapply(
        FUN = function(x)
          names(var2data.from)[match(x, var2data.from)]) %>%
        lapply(
          FUN = function(x)
            unique(var2data.to[x])) %>%
        lapply(function(x)
          if (length(x) > 1)
          paste(x, collapse = get_dataset_attr(which = "sep",
                                               dataset = dataset))
          else x) %>%
        unlist()
    } else {
      variables.data.output <- variables.data %>%
        lapply(
          FUN = function(x)
          names(var2data.from)[match(x, var2data.from)]) %>%
        lapply(
          FUN = function(x)
            var2data.to[x]) %>%
        lapply(function(x)
          if (length(x) > 1)
            paste(x, collapse = get_dataset_attr(which = "sep",
                                                 dataset = dataset))
          else x) %>%
        unlist()
    }

    return(variables.data.output)


  } else if (list.input) {

    var2data.from <- get_variables_data(variables = All,
                                        which = match.to,
                                        dataset = dataset)

    var2data.to <- get_variables_data(variables = All,
                                      which = which,
                                      dataset = dataset)

    if (unique.matches) {
      variables.data.output <- variables.data %>%
        lapply(
          FUN = function(x) names(var2data.from)[match(x, var2data.from)]) %>%
        lapply(
          FUN = function(x) var2data.to[x])
    } else {
      variables.data.output <- variables.data %>%
        lapply(
        FUN = function(x) names(var2data.from)[match(x, var2data.from)]) %>%
        lapply(
          FUN = function(x) unique(var2data.to[x]))
    }

    return(variables.data.output)

  } else {

    stop("Incorrect input, but it's not clear what.
         Please either provide a list or a vector of variables data.")

  }


}
