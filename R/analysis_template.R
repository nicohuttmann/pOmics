generic_analysis <- function(variables, observations, groups, observations.set, name, type, dataset) {

  # Get data
  data <- get_data(variables = !!dplyr::enquo(variables),
                   observations = !!dplyr::enquo(observations),
                   observations.set = observations.set,
                   name = name,
                   type = type,
                   dataset = dataset)

  # Get groups
  if (hasArg(groups)) {
    groups <- get_groups(observations = rownames(data),
                         groups = !!dplyr::enquo(groups),
                         observations.set = observations.set,
                         dataset = dataset)
  }

}
