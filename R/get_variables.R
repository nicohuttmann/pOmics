#' Return variables
#'
#' @param variables vector of variables
#' @param dataset dataset
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
get_variables <- function(variables, dataset) {

  # check dataset
  dataset <- get_dataset(dataset)

  # No variables specified
  if (!hasArg(variables)) return(.datasets[[dataset]][["variables"]] %>%
                                   dplyr::pull(var = "variables", name = NULL))


  # Check if input is vector
  vector.input <- tryCatch(is.atomic(variables),
                           error = function(cond) FALSE)


  # if variables input expression
  if (!vector.input) {
    return(.datasets[[dataset]][["variables"]] %>%
             dplyr::filter(!!dplyr::enquo(variables)) %>%
             dplyr::pull(var = "variables", name = NULL))

  # default
  } else if (length(variables) == 1 && variables == "default") {

    # No default
    if (is.na(get_dataset_attr(which = "default_variables", dataset = dataset)))
      stop("No default variables set.")

    variables.data <-
      get_variables_data(variables = All,
                         which = get_dataset_attr(which = "default_variables",
                                                  dataset = dataset),
                                         dataset = dataset) %>%
      na.omit()

    return(names(variables.data)[variables.data])


  # input given as vector
  # intersect given proteins with proteins in dataset
  } else {
    return(intersect(variables,
                     .datasets[[dataset]][["variables"]] %>%
                       dplyr::pull(var = "variables", name = NULL)))


  }


}

#' Return variables
#'
#' @param variables vector of variables
#' @param dataset dataset
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'

get_var <- get_variables
