#' Add data to variables
#'
#' @param data new data
#' @param name name
#' @param dataset dataset
#' @param set.default set new variables data as default
#' @param add.background.variable add name of column to the background
#' variables for easy access
#' @param replace replace existing column
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
add_variables_data <- function(data,
                               name,
                               dataset,
                               set.default = F,
                               add.background.variable = T,
                               replace) {

  # Check dataset
  dataset <- get_dataset(dataset)

  # Get template
  template <- get_variables_template(dataset)

  # Name
  name <- ask_name(name = name)


  # Fill template with data
  if (is.atomic(data) && length(names(data)) > 0) {
    template[names(data)] <- data
    # Data vector not named but given vector indicates variables
  } else if(is.atomic(data) && all(data %in% names(template))) {
    template[data] <- T
    # List which can be coerced to vector
  } else if (is.list(data) && all(sapply(data, length) == 1)) {
    data <- unlist(data)
    template[names(data)] <- data
    # list
  } else if (is.list(data)) {

    template <- as.list(template)

    for (i in intersect(names(template), names(data))) {
      template[[i]] <- data[[i]]
    }

    # Stop
  } else {
    message("Data cannot be added.")
    return(invisible(FALSE))
  }



  # ---- Name already present in dataset ----
  if (name %in% get_variables_data_names(dataset = dataset)) {

    # Argument replace given as TRUE
    if (hasArg(replace) && replace) {

      remove_variables_data(name = name,
                            dataset = dataset,
                            require.confirmation = FALSE)

      # No argument given for replace
    } else if (!hasArg(replace)) {

      # Ask
      message(paste0("Column <", name, "> already in variables data."))
      if (menu(choices = c("Yes", "No"),
               title = "Should column be replaced? ") == 1) {

        remove_variables_data(name = name,
                              dataset = dataset,
                              require.confirmation = FALSE)

      } else {
        message("Existing data has not been overwritten.")
        return(invisible(FALSE))
      }

    } else {
      message(paste0("Column <", name, "> already in variables data."))
      return(invisible(FALSE))
    }

  }



  # ---- Add ----
  .datasets[[dataset]][["variables"]] <<-
    .datasets[[dataset]][["variables"]] %>%
    dplyr::mutate(!!name := unname(template))


  # Set as default variables
  if (set.default) set_dataset_attr(x = name, which = "default_variables",
                                    dataset = dataset)


  # Add background variable
  if (add.background.variable) add_background_variable(name)

}
