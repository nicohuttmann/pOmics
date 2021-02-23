#' Adds data to observations dataframe
#'
#' @param data new data
#' @param name name
#' @param observations.set set of observations
#' @param dataset dataset
#' @param ignore.names Assumes that data matches observations
#' @param replace replace existing column
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
add_observations_data <- function(data, name, observations.set, dataset, ignore.names = F, replace) {

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

  if (is.null(names(data)) && !ignore.names) stop("Data must be named.")


  # Fill template with data
  if (!ignore.names) {
    template[names(data)] <- data
  } else {
    template[] <- data
  }

  # check name
  name <- ask_name(name, "Name for new data: ")

  # Name already present in dataset
  if (name %in% .datasets[[dataset]][["observations"]][[observations.set]]) {

    # Argument replace given as TRUE
    if (hasArg(replace) && replace) {

      remove_observations_data(name = name,
                               observations.set = observations.set,
                               dataset = dataset,
                               require.confirmation = FALSE)

    # No argument given for replace
    } else if (!hasArg(replace)) {

      # Ask
      if (menu(choices = c("Yes", "No"), title = "Should column be replaced? ") == 1) {

        remove_observations_data(name = name,
                                 observations.set = observations.set,
                                 dataset = dataset,
                                 require.confirmation = FALSE)

      } else {
        stop("Column with same name already exists.")
      }

    } else {
      stop("Column with same name already exists.")
    }

  }


  # Add
  .datasets[[dataset]][["observations"]][[observations.set]] <<-
    .datasets[[dataset]][["observations"]][[observations.set]] %>%
    dplyr::mutate(!!name := template)

}
