#' Return variables data
#'
#' @param variables (optional) vector of variables or expression
#' @param which which variables data to pull
#' @param dataset dataset
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
get_variables_data <- function(variables, which, dataset) {

  # check dataset
  dataset <- get_dataset(dataset)


  # Variables

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



  data <- .datasets[[dataset]][["variables"]] %>%
    dplyr::pull(var = !!dplyr::enquo(which), name = 1)

  return(data[variables])

}

#' Return variables data
#'
#' @param which which variables data to pull
#' @param variables (optional) vector of variables or expression
#' @param dataset dataset
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
get_var_data <- get_variables_data

