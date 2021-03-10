#' Add data to variables
#'
#' @param data new data
#' @param name name
#' @param dataset dataset
#' @param set.default set new variables data as default
#' @param add.background.variable add name of column to the background variables for easy access
#' @param replace replace existing column
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
add_variables_data <- function(data, name, dataset, set.default = F, add.background.variable = T, replace) {

  # Check dataset
  dataset <- get_dataset(dataset)

  # Get template
  template <- get_variables_template(dataset)

  # Name
  name <- ask_name(name = name)


  # Fill template with data
  if (length(names(data)) > 0) {
    template[names(data)] <- data
    # Data vector not named but given vector indicates variables
  } else if(all(data %in% names(template))) {
    template[data] <- T
    # Stop
  } else {
    stop("Data cannot be added.")
  }



  # Name already present in dataset
  if (name %in% colnames(.datasets[[dataset]][["variables"]])) {

    # Argument replace given as TRUE
    if (hasArg(replace) && replace) {

      remove_variables_data(name = name,
                            dataset = dataset,
                            require.confirmation = FALSE)

      # No argument given for replace
    } else if (!hasArg(replace)) {

      # Ask
      message("")
      message(paste0("Column <", name, "> already in variables data."))
      if (menu(choices = c("Yes", "No"), title = "Should column be replaced? ") == 1) {

        remove_variables_data(name = name,
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
  .datasets[[dataset]][["variables"]] <<- .datasets[[dataset]][["variables"]] %>%
    dplyr::mutate(!!name := template)


  # Set as default variables
  if (set.default) set_dataset_attr(x = name, which = "default_variables", dataset = dataset)


  # Add background variable
  if (add.background.variable) add_background_variable(name)

}
