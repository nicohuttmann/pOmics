#' Assembles variables
#'
#' @param variables variables defined by dplyr::filter
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


  # Return
  if (hasArg(variables)) {

    #
    return(.datasets[[dataset]][["variables"]] %>%
             dplyr::filter(!!dplyr::enquo(variables)) %>%
             dplyr::pull(variables))

    # If no variable if defined, take default
  } else {

    #
    return(.datasets[[dataset]][["variables"]] %>%
             dplyr::filter(!!rlang::sym(attr(.datasets[[dataset]], "default_variables"))) %>%
             dplyr::pull(variables))

  }

}
