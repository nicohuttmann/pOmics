#' Assembles observations
#'
#' @param observations observations defined by dplyr::filter
#' @param observations.set observations data frame
#' @param dataset datset
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
get_observations <- function(observations, observations.set, dataset) {

  # check dataset
  dataset <- get_dataset(dataset)

  # Check observations.set
  observations.set <- get_observations_set(observations.set, dataset)

  # Return
  if (hasArg(observations)) {

    #
    return(.datasets[[dataset]][["observations"]][[observations.set]] %>%
             dplyr::filter(!!dplyr::enquo(observations)) %>%
             dplyr::pull(observations))

    # If no variable if defined, take default
  } else {

    #
    return(.datasets[[dataset]][["observations"]][[observations.set]] %>%
             dplyr::filter(!!rlang::sym(attr(.datasets[[dataset]], "default_observations"))) %>%
             dplyr::pull(observations))

  }

}
