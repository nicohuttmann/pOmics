#' Assemble data from dataset and return in list
#'
#' @param which specific name of data type
#' @param variables selected variables
#' @param observations selected observations
#' @param output name to save in list
#' @param output.type output type ("list" (default), "tibble", "data.frame", "matrix")
#' @param observations.set set of observations
#' @param dataset dataset name or number
#'
#' @return
#' @export
#'
#'
get_data <- function(which, variables = "default", observations = "default", output, output.type = "list",
                      observations.set, dataset) {

  # Checks correct name of dataset
  dataset <- get_dataset(dataset)

  # Check data type and name
  which <- get_data_name(name = which,
                             dataset = dataset)

  # Assemble variables
  variables <- get_variables(variables = {{variables}},
                             dataset = dataset)

  # Assemble observations
  observations <- get_observations(observations = {{observations}},
                                   observations.set = observations.set,
                                   dataset = dataset)


  data <- .datasets[[dataset]][[which]]



  if (output.type == "list") {

    if (!hasArg(output)) output <- which

    # Add data to list
    data_ <- tibble::lst(!!output := data %>%
                           dplyr::filter(observations %in% !!observations) %>%
                           dplyr::select(c(observations, dplyr::any_of(variables))))

  } else if (output.type == "tibble") {

    data_ <- data %>%
      dplyr::filter(observations %in% !!observations) %>%
      dplyr::select(c(observations, dplyr::any_of(variables)))

  } else if (output.type == "data.frame") {

    data_ <- data %>%
      dplyr::filter(observations %in% !!observations) %>%
      dplyr::select(c(observations, dplyr::any_of(variables))) %>%
      tibble2data.frame()

  } else if (output.type == "matrix") {

    data_ <- data %>%
      dplyr::filter(observations %in% !!observations) %>%
      dplyr::select(c(observations, dplyr::any_of(variables))) %>%
      tibble2matrix()

  } else {

    message(paste0("Output type <",
                   output.type,
                   "> not supported. Use <list>, <tibble>, <data.frame> or <matrix> instead."))
    invisible(NULL)

  }

  # Return
  return(data_)

}
