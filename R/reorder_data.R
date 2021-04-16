#' Reorders dataframe rows according to observations data order
#'
#' @param data.name name of data
#' @param observations.set observations set
#' @param dataset dataset
#' @param silent return error if function fails
#'
#' @return
#' @export
#'
#'
reorder_data <- function(data.name, observations.set, dataset, silent = F) {

  # Check dataset
  dataset <- get_dataset(dataset)

  # Check observation set
  observations.set <- get_observations_set(observations.set = observations.set,
                                           dataset = dataset)
  # Data name
  data.name <- get_data_name(name = data.name, dataset = dataset)

  #
  observations <- get_observations(observations.set = observations.set,
                                   dataset = dataset)

  # Check observations
  if (!all(.datasets[[dataset]][[data.name]][["observations"]] %in% observations)) {

    if (!silent) {

      stop("Dataset contains unknown observations.")

    } else {

    }

  } else {

    # Rearrange data frame
  .datasets[[dataset]][[data.name]] <<-
    .datasets[[dataset]][[data.name]] %>%
    dplyr::arrange(match(observations, !!observations))

  }

}
