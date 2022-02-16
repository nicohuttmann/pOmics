#' Return observations data
#'
#' @param which which observations data to pull
#' @param observations (optional) vector of observations or expression
#' @param observations.set observations data frame
#' @param dataset dataset
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
get_observations_data <- function(which,
                                  observations,
                                  observations.set,
                                  dataset) {

  # check dataset
  dataset <- get_dataset(dataset)

  # Check observations set
  observations.set <- get_observations_set(observations.set = observations.set,
                                           dataset = dataset)

  # Observations

  # No observations defined
  if (!hasArg(observations)) {

    observations <- get_observations(observations.set = observations.set,
                                     dataset = dataset)

    # Variables defined
  } else {

    # Check if input is vector
    vector.input <- tryCatch(is.atomic(observations),
                             error = function(cond) FALSE)

    # Default observations
    if (vector.input &&
        length(observations) == 1 && observations == "default") {
      observations <-
        get_observations(observations = "default",
                         observations.set = observations.set,
                         dataset = dataset)
    }


    # if observations input is vector
    if (!vector.input) {
      observations <-
        get_observations(observations = !!dplyr::enquo(observations),
                         observations.set = observations.set,
                         dataset = dataset)
    }

  }



  data <- .datasets[[dataset]][["observations"]][[observations.set]] %>%
    dplyr::pull(var = !!dplyr::enquo(which), name = 1)

  return(data[observations])

}

#' Return observations data
#'
#' @param which which observations data to pull
#' @param observations (optional) vector of observations or expression
#' @param observations.set observations data frame
#' @param dataset dataset
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
get_obs_data <- get_observations_data
