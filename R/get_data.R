#' Assemble data from dataset and return in list
#'
#' @param which specific name of data type
#' @param variables selected variables
#' @param observations selected observations
#' @param output name to save in list
#' @param output.type output type (default = "tibble_inlist"; combination of
#' "tibble", "data.frame" or "matrix" to define data type and "_inlist" or ""
#' if data should be put into a list or not)
#' @param observations.set set of observations
#' @param dataset dataset name or number
#'
#' @return
#' @export
#'
#'
get_data <- function(which,
                     variables = "default",
                     observations = "default",
                     output,
                     output.type = "tibble_inlist",
                     observations.set,
                     dataset) {

  # Checks correct name of dataset
  dataset <- get_dataset(dataset)

  # Default data name
  if (!hasArg(which)) {
    which <- get_data_name(name = which, dataset = dataset)
  }

  # Check data type and name
  # Check if names are all in variables data
  if (any(!which %in% get_data_names(dataset = dataset))) {

    message(paste0("Some data names were not found: \n  ",
                   paste(which[!which %in% get_data_names(dataset = dataset)],
                         collapse = "\n  ")))

    # Transfer data if possible
    if (any(which[!which %in%
                  get_data_names(dataset = dataset)] %in%
            available_data_frames(dataset = dataset,
                                     view = F,
                                     return = T,
                                     print.call = F))) {

      # Message which data is transferred
      message(paste0(
        "\nFollowing names were found in raw data and will be transferred: ",
        "\n  ",
        paste(which[which %in% available_data_frames(dataset = dataset,
                                                     view = F,
                                                     return = T,
                                                     print.call = F) &
                      !which %in% get_data_names(dataset = dataset)],
              collapse = "\n  ")))

      # Transfer missing variables data
      transfer_data_frames(
        dataset = dataset,
        data.columns = which[which %in%
                               available_data_frames(dataset = dataset,
                                                     view = F,
                                                     return = T,
                                                     print.call = F) &
                               !which %in% get_data_names(dataset = dataset)])


    }

  }



  # Assemble variables
  variables <- get_variables(variables = {{variables}},
                             dataset = dataset)

  # Assemble observations
  observations <- get_observations(observations = {{observations}},
                                   observations.set = observations.set,
                                   dataset = dataset)






  data <- .datasets[[dataset]][[which]]



  if (grepl(pattern = "tibble", x = output.type)) {

    data <- data %>%
      dplyr::filter(observations %in% !!observations) %>%
      dplyr::select(c(observations, dplyr::any_of(variables)))

  } else if (grepl(pattern = "data.frame", x = output.type)) {

    data <- data %>%
      dplyr::filter(observations %in% !!observations) %>%
      dplyr::select(c(observations, dplyr::any_of(variables))) %>%
      tibble2data.frame()

  } else if (grepl(pattern = "matrix", x = output.type)) {

    data <- data %>%
      dplyr::filter(observations %in% !!observations) %>%
      dplyr::select(c(observations, dplyr::any_of(variables))) %>%
      tibble2matrix()

  } else {

    message(paste0("Output type <",
                   output.type,
                   "> not supported. Use <tibble>, <data.frame> ",
                   "or <matrix> instead with the optional suffix <_inlist>."))

    return(invisible(NULL))

  }


  if (grepl(pattern = "_inlist", x = output.type)) {

    if (!hasArg(output)) output <- which

    # Add data to list
    data_ <- tibble::lst(
      !!output := data)

  } else {
    data_ <- data
  }



  # Return
  return(data_)

}
