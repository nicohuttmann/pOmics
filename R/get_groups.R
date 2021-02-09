#' Returns groups vector
#'
#' @param groups groups
#' @param control (optional) which group is control
#' @param observations observations
#' @param observations.set which observations.set to use
#' @param dataset dataset
# '
#' @return
#' @export
#'
#'
get_groups <- function(groups, control, observations, observations.set, dataset) {

  # Check dataset
  dataset <- get_dataset(dataset)

  # Check observations set
  observations.set <- get_observations_set(observations.set = observations.set,
                                           dataset = dataset)

  # Check if input is vector
  vector.input <- tryCatch(is.vector(observations),
                           error = function(cond) FALSE)


  # if variables input is vector
  if (!vector.input) {
    observations <- get_observations(observations = !!dplyr::enquo(observations),
                                     observations.set = observations.set,
                                     dataset = dataset)
  }


  # Get groups data
  group.vector <- .datasets[[dataset]][["observations"]][[observations.set]] %>%
    dplyr::pull(!!dplyr::enquo(groups))



  # Check observations and groups
  if (any(!observations %in% names(group.vector))) stop("Groups do not contain all observations.")


  # Remove observations
  group.vector <- group.vector[observations]




  # Define order of groups

  # More than two groups
  if (!hasArg(control)) {

    group.factors <- factor(group.vector)

    # control defined but not in groups
  } else if (hasArg(control) && !(control %in% group.vector)) {

    stop("Control not found in groups.")

    # Control group defined
  } else if(hasArg(control) && control %in% group.vector) {

    group.factors <- factor(group.vector, levels = c(control, setdiff(unique(group.vector), control)))

    #
  } else {
    stop("Something went wrong.")
  }


  # Return
  return(group.factors)

}
