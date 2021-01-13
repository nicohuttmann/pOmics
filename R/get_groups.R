get_groups <- function(groups, observations.set, dataset, auto.order = T) {

  # Check dataset
  dataset <- get_dataset(dataset)

  # Check observations set
  observations.set <- get_observations_set(observations.set = observations.set,
                                           dataset = dataset)


  #


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
