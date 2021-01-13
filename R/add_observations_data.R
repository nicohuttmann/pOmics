#' Adds data to observations dataframe
#'
#' @param data new data
#' @param name name
#' @param observations.set set of observations
#' @param dataset dataset
#' @param ignore.names Assumes that data matches observations
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
add_observations_data <- function(data, name, observations.set, dataset, ignore.names = F) {

  # Check dataset
  dataset <- get_dataset(dataset)

  # Check observation set
  observations.set <- get_observations_set(observations.set = observations.set,
                                           dataset = dataset)


  # Get template
  template <- get_observations_template(observations.set = observations.set,
                                        dataset = dataset)


  # Check data
  if (!hasArg(data)) stop("No data given.")
  if (!hasArg(name)) stop("No name given.")
  if (is.null(names(data)) && !ignore.names) stop("Data must be named.")


  # Fill template with data
  if (!ignore.names) {
    template[names(data)] <- data
  } else {
    template[] <- data
  }



  # Add
  .datasets[[dataset]][["observations"]][[observations.set]] <<- .datasets[[dataset]][["observations"]][[observations.set]] %>%
    dplyr::mutate(new = template) %>%
    dplyr::rename(!!name := new)

}
