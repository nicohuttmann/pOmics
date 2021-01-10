#' Assembles observations
#'
#' @param dataset datset
#' @param observations.set observations data frame
#' @param observations observations defined by dplyr::filter
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
get_observations <- function(dataset, observations.set, observations) {

  # check dataset
  dataset <- get_dataset(dataset)

  # Check observations.set
  observations.set <- get_observations_set(dataset, observations.set)

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
