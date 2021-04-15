#' Adds data to observations dataframe
#'
#' @param data new data
#' @param name name
#' @param observations.set set of observations
#' @param dataset dataset
#' @param ignore.names Assumes that data matches observations
#' @param add.background.variable add name of column to the background variables for easy access
#' @param replace replace existing column
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
add_observations_data <- function(data, name, observations.set, dataset, ignore.names = F, add.background.variable = T,
                                  replace) {

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
  if (is.factor(data) & all((names(data) == names(template)))) {
    template <- data
  } else if (is.factor(data)) {
    stop("Please make sure all observations match when you provide factors.")
  } else if (!ignore.names) {
    template[names(data)] <- data
  } else {
    template[] <- data
  }


  # check name
  name <- ask_name(name, "Name for new data: ")


  # Name already present in dataset
  if (name %in% colnames(.datasets[[dataset]][["observations"]][[observations.set]])) {

    # Argument replace given as TRUE
    if (hasArg(replace) && replace) {

      remove_observations_data(name = name,
                               observations.set = observations.set,
                               dataset = dataset,
                               require.confirmation = FALSE)

    # No argument given for replace
    } else if (!hasArg(replace)) {

      # Ask
      message("")
      message(paste0("Column <", name, "> already in observations data."))
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


  # Add background variable
  if (add.background.variable) add_background_variable(name)


}
