#' Return observations data
#'
#' @param name name of data
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
get_observations_data <- function(name, observations, observations.set, dataset) {

  # check dataset
  dataset <- get_dataset(dataset)

  # Check observations set
  observations.set <- get_observations_set(observations.set = observations.set)

  # Observations

  # No observations defines
  if (!hasArg(observations)) {

    observations <- get_observations()

    # Variables defined
  } else {

    # Check if input is vector
    vector.input <- tryCatch(is.vector(observations),
                             error = function(cond) FALSE)

    # Default observations
    if (vector.input && length(observations) == 1 && observations == "default") {
      observations <- get_observations(observations = "default", dataset = dataset)
    }


    # if observations input is vector
    if (!vector.input) {
      observations <- get_observations(observations = !!dplyr::enquo(observations), dataset = dataset)
    }

  }



  data <- .datasets[[dataset]][["observations"]][[observations.set]] %>%
    dplyr::pull(var = !!dplyr::enquo(name), name = 1)

  return(data[observations])

}
