#' Return observations
#'
#' @param observations vector of observations
#' @param observations.set observations data frame
#' @param dataset dataset
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


  # No observations specified
  if (!hasArg(observations))
    return(.datasets[[dataset]][["observations"]][[observations.set]] %>%
             dplyr::pull(var = "observations", name = NULL))

  # Check if input is vector
  vector.input <- tryCatch(is.atomic(observations),
                           error = function(cond) FALSE)

  # if observations input expression
  if (!vector.input) {
    return(.datasets[[dataset]][["observations"]][[observations.set]] %>%
             dplyr::filter(!!dplyr::enquo(observations)) %>%
             dplyr::pull(var = "observations", name = NULL))

    # default
  } else if (length(observations) == 1 && observations == "default") {

    # No default
    if (is.na(get_dataset_attr(which = "default_observations",
                               dataset = dataset)))
      stop("No default observations set.")

    observations.data <-
      get_observations_data(
        which = get_dataset_attr(
          which = "default_observations",
          dataset = dataset),
        observations = All,
        observations.set = observations.set,
        dataset = dataset) %>%
      na.omit()

    return(names(observations.data)[observations.data])


    # input given as vector
    # intersect given proteins with proteins in dataset
  } else {
    return(
      intersect(observations,
                .datasets[[dataset]][["observations"]][[observations.set]] %>%
                  dplyr::pull(var = "observations", name = NULL)))


  }


}

#' Return observations
#'
#' @param observations vector of observations
#' @param observations.set observations data frame
#' @param dataset dataset
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
get_obs <- get_observations
