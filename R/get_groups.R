#' Returns groups vector
#'
#' @param observations observations
#' @param groups groups
#' @param control (optional) which group is control
#' @param observations.set which observations.set to use
#' @param dataset dataset
#' @param control.first (optional) should first group be set as control
# '
#' @return
#' @export
#'
#'
get_groups <- function(observations, groups, control, observations.set, dataset, control.first = F) {

  # Check dataset
  dataset <- get_dataset(dataset)

  # Check observations set
  observations.set <- get_observations_set(observations.set = observations.set,
                                           dataset = dataset)


  # Return
  if (hasArg(groups)) {

    #
    group.vector <- .datasets[[dataset]][["observations"]][[observations.set]] %>%
      dplyr::pull(var = !!dplyr::enquo(groups), name = observations)

    # If no variable if defined, take default
  } else {

    #
    group.vector <- .datasets[[dataset]][["observations"]][[observations.set]] %>%
      dplyr::pull(var = !!rlang::sym(attr(.datasets[[dataset]], paste0("default_", observations.set, "groups"))),
                  name = observations)

  }


  group.vector <- na.omit(group.vector)


  # Check observations
  if (any(!observations %in% names(group.vector))) stop("Group definition does not contain all observations.")

  else group.vector <- group.vector[observations]



  # Check order
  if (length(unique(group.vector)) > 2 && !hasArg(control) && !control.first) {

    group.factors <- factor(group.vector)

  } else if(hasArg(control) && control %in% group.vector && !control.first) {

    group.factors <- factor(group.vector, levels = c(control, setdiff(unique(group.vector), control)))
  } else if(hasArg(control) && control.first) {

    stop("Only one mode of ordering can be used.")
  } else if (control.first) {

    group.factors <- factor(group.vector, levels = unique(group.vector))

  } else {
    stop("Group assignment not correct.")
  }


  # Return
  return(group.factors)

}
