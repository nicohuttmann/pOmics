#' Adds data to observations data frame
#'
#' @param data new data
#' @param name name
#' @param observations.set set of observations
#' @param dataset dataset
#' @param order.by orders observations data by given vector/factor levels
#' @param ignore.names Assumes that data matches observations
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
add_observations_data <- function(data,
                                  name,
                                  observations.set,
                                  dataset,
                                  order.by = F,
                                  ignore.names = F,
                                  add.background.variable = T,
                                  replace) {

  # Add: Multiple datasets given
  if (hasArg(dataset) && length(dataset) > 1) {
    for (i in dataset) {
      add_observations_data(data = data,
                            name = name,
                            observations.set = observations.set,
                            dataset = i,
                            order.by = order.by,
                            ignore.names = ignore.names,
                            add.background.variable = add.background.variable,
                            replace = replace)
    }
  } else {

    # Check dataset
    dataset <- get_dataset(dataset)

    # Check observation set
    observations.set <- get_observations_set(observations.set = observations.set,
                                             dataset = dataset)


    # Get template
    template <- get_observations_template(observations.set = observations.set,
                                          dataset = dataset)


    # Check data
    if (!hasArg(data)) {
      message("No data given.")
      return(invisible(FALSE))
    }

    if (is.null(names(data)) && !ignore.names) {
      message("Data must be named.")
      return(invisible(FALSE))
    }


    # Fill template with data
    if (is.factor(data) &
        length(names(data)) == length(data) &
        all((names(data) %in% names(template)))) {
      template <- factor(data[names(template)])
    } else if (is.factor(data)) {
      message("Make sure all observations are named when you provide factors.")
    } else if (!ignore.names) {
      template[names(data)] <- data
    } else {
      template[] <- data
    }


    # check name
    name <- ask_name(name, "Name for new data: ")


    # Name already present in dataset
    if (name %in%
        colnames(.datasets[[dataset]][["observations"]][[observations.set]])) {

      # Argument replace given as TRUE
      if (hasArg(replace) && replace) {

        remove_observations_data(name = name,
                                 observations.set = observations.set,
                                 dataset = dataset,
                                 require.confirmation = FALSE)

        # No argument given for replace
      } else if (!hasArg(replace)) {

        # Ask
        message(paste0("Column <", name, "> already in observations data."))

        if (menu(choices = c("Yes", "No"),
                 title = "Should column be replaced? ") == 1) {

          remove_observations_data(name = name,
                                   observations.set = observations.set,
                                   dataset = dataset,
                                   require.confirmation = FALSE)

        } else {
          message("Existing data has not been overwritten.")
          return(invisible(FALSE))
        }

      } else {
        message(paste0("Column <", name, "> already in observations data."))
        return(invisible(FALSE))
      }

    }


    # Add
    .datasets[[dataset]][["observations"]][[observations.set]] <<-
      .datasets[[dataset]][["observations"]][[observations.set]] %>%
      dplyr::mutate(!!name := template)


    # Order observations data
    if (order.by) {

      .datasets[[dataset]][["observations"]][[observations.set]] <<-
        .datasets[[dataset]][["observations"]][[observations.set]] %>%
        dplyr::arrange(!!sym(eval(name)))

      for (i in get_data_names(dataset)) {

        reorder_data(data.name = i,
                     observations.set = observations.set,
                     dataset = dataset,
                     silent = TRUE)

      }

    }


    # Add background variable
    if (add.background.variable) add_background_variable(name)

  }


}
