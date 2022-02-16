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



  # Grab data
  data <- .datasets[[dataset]][[which]]


  #
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




  # Make data sticky
  data <- sticky::sticky(data)

  # Add attributes
  attr(data, "dataset") <- dataset
  attr(data, "data") <- which
  attr(data, "variables") <- variables
  attr(data, "observations") <- observations
  attr(data, "rows") <- "observations"
  attr(data, "columns") <- "variables"

  data <- .update_data_attributes(data)


  # Return in list or not
  if (grepl(pattern = "_inlist", x = output.type)) {

    if (!hasArg(output)) output <- which

    # Add data to list
    data_ <- tibble::lst(!!output := data)

    # Transfer attributes
    attr(data_, "dataset") <- dataset
    attr(data_, "data") <- output

  } else {
    data_ <- data
  }


  # Return
  return(data_)

}


#' Assemble data from dataset and return in list
#'
#' @param data_ data_ list
#' @param which specific name of data type
#' @param variables selected variables
#' @param observations selected observations
#' @param observations.set set of observations
#' @param dataset dataset name or number
#' @param output name to save in list
#'
#' @return
#' @export
#'
#'
put_data <- function(data_,
                     which,
                     variables = "default",
                     observations = "default",
                     observations.set,
                     dataset,
                     output) {

  # Check input
  if (!hasArg(data_) || !is.list(data_))
    stop("No list given to which data can be added.")

  if (!hasArg(output)) output <- which

  if (output %in% names(data_))
    stop("Name data name already present in list. Please provide a unque name.")


  # Checks correct name of dataset
  if (!hasArg(dataset)) dataset <- attr(data_, "dataset")
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


  # Make data sticky
  data <- sticky::sticky(data)

  # Add attributes
  attr(data, "dataset") <- dataset
  attr(data, "data") <- which
  attr(data, "variables") <- variables
  attr(data, "observations") <- observations
  attr(data, "rows") <- "observations"
  attr(data, "columns") <- "variables"


  # Add data to list
  data_[[output]] <- data %>%
    dplyr::filter(observations %in% !!observations) %>%
    dplyr::select(c(observations, dplyr::any_of(variables)))


  # ---- Transfer attributes ----
  attr(data_[[output]], "dataset") <- dataset
  attr(data_, "dataset") <- dataset
  attr(data_, "data") <- output


  # Return
  return(data_)

}


#' Return variables data
#'
#' @param which which variables data to pull (multiple supported)
#' @param variables (optional) vector of variables or expression
#' @param output.type output type (default = "vector" or "tibble_inlist" for
#' multiple arguments; combination of "vector", "list", "tibble", or
#' "data.frame" to define data type and "_inlist" or "" if data should be put
#' into a list or not)
#' @param ... arguments for FUN function
#' @param dataset dataset
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
get_variables_data <- function(which,
                               variables,
                               output.type = "vector",
                               ...,
                               dataset) {

  # check dataset
  dataset <- get_dataset(dataset = dataset)


  ### Variables

  # No variables defined
  if (!hasArg(variables)) {

    variables <- get_variables(dataset = dataset)

    # Variables defined
  } else {

    # Check if input is vector
    vector.input <- tryCatch(is.atomic(variables),
                             error = function(cond) FALSE)

    # Default variables
    if (vector.input && length(variables) == 1 && variables == "default") {
      variables <- get_variables(variables = "default", dataset = dataset)
    }


    # if variables input is vector
    if (!vector.input) {
      variables <- get_variables(variables = !!dplyr::enquo(variables),
                                 dataset = dataset)
    }

  }


  # Which data to pull

  # No argument given (which)
  if (!hasArg(which)) {
    which <- select.list(
      choices = get_variables_data_names(dataset = dataset),
      multiple = TRUE,
      title = "Select variables data columns:")

    # Test which
    if (length(which) == 0) {
      return(NULL)
    }

  }


  # Check if names of data were not found in variables_data
  if (any(!which %in% get_variables_data_names(dataset = dataset))) {

    message(paste0("Some data names were not found: \n  ",
                   paste(which[!which %in%
                                 get_variables_data_names(dataset = dataset)],
                         collapse = "\n  ")))

    # Transfer variables data if possible
    if (any(which[!which %in%
                  get_variables_data_names(dataset = dataset)] %in%
            available_variables_data(dataset = dataset,
                                     view = F,
                                     return = T,
                                     print.call = F))) {

      # Message which variables data is transferred
      message(paste0(
        "\nFollowing names were found in raw data and will be transferred: ",
        "\n  ",
        paste(which[which %in% available_variables_data(dataset = dataset,
                                                        view = F,
                                                        return = T,
                                                        print.call = F) &
                      !which %in% get_variables_data_names(dataset = dataset)],
              collapse = "\n  ")))

      # Transfer missing variables data
      transfer_variables_data(
        dataset = dataset,
        data.columns = which[which %in%
                               available_variables_data(dataset = dataset,
                                                        view = F,
                                                        return = T,
                                                        print.call = F) &
                               !which %in%
                               get_variables_data_names(dataset = dataset)])


    }

  }


  # Adjust default for multiple variables data
  if (length(which) > 1 && grepl(pattern = "vector", x = output.type)) {
    output.type <- "tibble_inlist"
  }


  # One variables data required
  if (length(which) == 1) {

    if (grepl(pattern = "vector", x = output.type)) {

      data <- .datasets[[dataset]][["variables"]] %>%
        dplyr::filter(.data[["variables"]] %in% !!variables) %>%
        dplyr::arrange(match(.data[["variables"]], !!variables)) %>%
        dplyr::pull(var = !!dplyr::enquo(which), name = "variables")


      # List
    } else if (regexpr(pattern = "list", text = output.type) == 1) {

      data <- .datasets[[dataset]][["variables"]] %>%
        dplyr::filter(.data[["variables"]] %in% !!variables) %>%
        dplyr::arrange(match(.data[["variables"]], !!variables)) %>%
        dplyr::pull(var = !!dplyr::enquo(which), name = "variables") %>%
        as.list()

      # Tibble
    } else if (grepl(pattern = "tibble", x = output.type)) {

      data <- .datasets[[dataset]][["variables"]] %>%
        dplyr::filter(.data[["variables"]] %in% !!variables) %>%
        dplyr::arrange(match(.data[["variables"]], !!variables)) %>%
        dplyr::select("variables", !!which)

    }

    # Multiple variables data names
  } else if (grepl(pattern = "tibble", x = output.type)) {

    data <- .datasets[[dataset]][["variables"]] %>%
      dplyr::filter(.data[["variables"]] %in% !!variables) %>%
      dplyr::arrange(match(.data[["variables"]], !!variables)) %>%
      dplyr::select("variables", !!which)


  } else if (grepl(pattern = "data.frame", x = output.type)) {

    data <- .datasets[[dataset]][["variables"]] %>%
      dplyr::filter(.data[["variables"]] %in% !!variables) %>%
      dplyr::arrange(match(.data[["variables"]], !!variables)) %>%
      dplyr::select("variables", !!which) %>%
      tibble2data.frame(from.row.names = "variables")

    # Output type not found
  } else {

    message(paste0("Output type <",
                   output.type,
                   "> not supported. Use <vector> and <list> for single ",
                   "variables data calls or <tibble> and <data.frame> for ",
                   "single and multiple variables data calls with the ",
                   "optional suffix <_inlist>."))

    return(invisible(NULL))

  }


  # Make data sticky
  data <- sticky::sticky(data)

  # Add attributes
  attr(data, "dataset") <- dataset
  attr(data, "data") <- "variables_data"
  attr(data, "variables") <- variables
  attr(data, "other") <- which

  if (is.data.frame(data)) {

    attr(data, "columns") <- "other"
    attr(data, "rows") <- "variables"

  }


  # Put data in list
  if (grepl(pattern = "_inlist", x = output.type)) {

    if (!hasArg(output)) output <- ifelse(length(which) == 1,
                                          which,
                                          "variables_data")

    # Add data to list
    data_ <- tibble::lst(
      !!output := data)

  } else {
    data_ <- data
  }


  # Return data
  return(data_)

}


#' Adds group vector to data frame
#'
#' @param data_ data frame
#' @param which observations data
#' @param name (optional) name of new column
#' @param observations.set which observations.set to use
#' @param dataset dataset
#' @param input data frame to be modified
#' @param output data frame to return in list
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
include_observations_data <- function(data_,
                                      which,
                                      name,
                                      observations.set,
                                      dataset,
                                      input,
                                      output) {

  # Handle input
  input_list <- data_input(data_ = data_, input = input)

  if (input_list[["error"]]) return(invisible(input_list[["data"]]))

  else {
    data <- input_list[["data"]]
    input <- input_list[["input"]]
    data_attributes <- input_list[["data_attributes"]]
  }


  # Matrix to tibble
  if (!tibble::is_tibble(data))
    data <- data2tibble(data = data, to.row.names = "observations")


  # Check dataset
  if (!hasArg(dataset)) dataset <- attr(data, "dataset")

  dataset <- get_dataset(dataset)


  # Check observations set
  observations.set <- get_observations_set(observations.set = observations.set,
                                           dataset = dataset)



  # Get groups data
  data.vector <- .datasets[[dataset]][["observations"]][[observations.set]] %>%
    dplyr::pull(var = !!which, name = "observations")


  # Get observations
  observations <- dplyr::pull(data, var = "observations")


  # Check observations and groups
  if (any(!observations %in% names(data.vector)))
    stop("Data does not contain all observations.")


  # Remove observations
  data.vector <- data.vector[observations]


  # Use input as name for new column
  if (!hasArg(name)) {

    if (!tryCatch(is.character(which),

                  error = function(cond) FALSE)) {

      name <- deparse(substitute(which))

    } else {

      name <- which

    }

  }


  # Add groups
  data <- dplyr::mutate(data, !!name := data.vector,
                        .before = .data_columns(data_attributes))


  # Reset attributes
  data <- data %>%
    .set_data_attributes(data_attributes) %>%
    .add_data_attributes(which = "observations_data", new_attr = name)

  # Output name
  if (!hasArg(output)) output <- input

  # Prepare return
  if (input_list[["list.input"]]) {
    data_[[output]] <- data
    attr(data_, "data") <- output
  }

  else data_ <- data

  # Return
  return(data_)

}


#' Adds group vector to data frame
#'
#' @param data_ analysis list
#' @param groups groups
#' @param control (optional) which group is control
#' @param observations.set which observations.set to use
#' @param dataset dataset
#' @param input name of input data
#' @param output name of output data
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
include_groups <- function(data_,
                           groups = "default",
                           control,
                           observations.set,
                           dataset,
                           input,
                           output) {

  # Handle input
  input_list <- data_input(data_ = data_, input = input)

  if (input_list[["error"]]) return(invisible(input_list[["data"]]))

  else {
    data <- input_list[["data"]]
    input <- input_list[["input"]]
    data_attributes <- input_list[["data_attributes"]]
  }


  # Matrix to tibble
  if (!tibble::is_tibble(data))
    data <- data2tibble(data = data, to.row.names = "observations")


  # Check dataset
  if (!hasArg(dataset)) dataset <- attr(data, "dataset")

  dataset <- get_dataset(dataset)


  # Check observations set
  observations.set <- get_observations_set(observations.set = observations.set,
                                           dataset = dataset)


  # Default groups
  if (groups == "default") {

    groups <- get_dataset_attr(which = "default_groups", dataset = dataset)

    if (is.null(groups)) {

      stop("Setup default groups data or provide a groups argument.")

    }

  }



  # Get groups data
  group.vector <- .datasets[[dataset]][["observations"]][[observations.set]] %>%
    dplyr::pull(var = !!dplyr::enquo(groups), name = "observations")


  # Get observations
  observations <- dplyr::pull(data, var = observations)


  # Check observations and groups
  if (any(!observations %in% names(group.vector)))
    stop("Groups do not contain all observations.")


  # Remove observations
  group.vector <- group.vector[observations]




  # Define order of groups

  # More than two groups
  if (!hasArg(control)) {

    group.factors <- factor(group.vector)

    # control defined but not in groups
  } else if (hasArg(control) && !(control %in% group.vector)) {

    stop("Control not found in groups.")

    # Control group defined
  } else if(hasArg(control) && control %in% group.vector) {

    group.factors <- factor(group.vector,
                            levels = c(control,
                                       setdiff(unique(group.vector),
                                               control)))

    #
  } else {
    stop("Something went wrong.")
  }


  # Add groups
  data <- dplyr::mutate(data, groups = group.factors,
                        .before = .data_columns(data_attributes))



  # Output name
  if (!hasArg(output)) output <- input

  # Prepare return
  if (input_list[["list.input"]]) {
    data_[[output]] <- data %>%
      .set_data_attributes(data_attributes) %>%
      .add_data_attributes(which = "observations_data", new_attr = "groups")
    attr(data_, "data") <- output
  }

  else data_ <- data %>%
    .set_data_attributes(data_attributes) %>%
    .add_data_attributes(which = "observations_data", new_attr = "groups")

  # Return
  return(data_)

}


#' Adds group vector to data frame
#'
#' @param data_ data frame
#' @param which variables data
#' @param name name of new column
#' @param dataset dataset
#' @param input data frame to be modified
#' @param output data frame to return in list
#'
#' @return
#' @export
#'
include_variables_data <- function(data_,
                                   which,
                                   name,
                                   dataset,
                                   input,
                                   output) {

  # Handle input
  input_list <- data_input(data_ = data_, input = input)

  if (input_list[["error"]]) return(invisible(input_list[["data"]]))

  else {
    data <- input_list[["data"]]
    input <- input_list[["input"]]
    data_attributes <- input_list[["data_attributes"]]
  }

  # Matrix to tibble
  if (!tibble::is_tibble(data))
    data <- data2tibble(data = data, to.row.names = "variables")

  # Check dataset
  if (!hasArg(dataset)) dataset <- attr(data, "dataset")

  dataset <- get_dataset(dataset)



  # Get variables data
  data.vector <- .datasets[[dataset]][["variables"]]%>%
    dplyr::pull(var = !!which, name = "variables")


  # Get variables
  variables <- dplyr::pull(data, var = "variables")


  # Check variables and groups
  if (any(!variables %in% names(data.vector)))
    stop("Data does not contain all variables.")


  # Remove variables
  data.vector <- data.vector[variables]


  # Use input as name for new column
  if (!hasArg(name)) {

    if (!tryCatch(is.character(which),
                  error = function(cond) FALSE)) {

      name <- deparse(substitute(which))

    } else {

      name <- which

    }

  }


  # Add data
  data <- dplyr::mutate(data, !!name := data.vector,
                        .before = .data_columns(data_attributes))

  # Reset attributes
  data <- data %>%
    .set_data_attributes(data_attributes) %>%
    .add_data_attributes(which = "variables_data", new_attr = name)


  # Output name
  if (!hasArg(output)) output <- input

  # Prepare return
  if (input_list[["list.input"]]) {
    data_[[output]] <- data
    attr(data_, "data") <- output
  }

  else data_ <- data

  # Return
  return(data_)

}


#' Removes columns from data frame based on minimum values above 0
#'
#' @param data_ data
#' @param variables variables
#' @param dataset dataset
#' @param input if data_ is list: name of data to use
#' @param output if data_ is list: name of output data to save in list under
#'
#' @return
#' @export
#'
#'
subset_variables <- function(data_, variables, dataset, input, output) {

  # Handle input
  input_list <- data_input(data_ = data_, input = input)

  if (input_list[["error"]]) return(invisible(input_list[["data"]]))

  else {
    data <- input_list[["data"]]
    input <- input_list[["input"]]
    data_attributes <- input_list[["data_attributes"]]
  }


  # Check dataset
  if (!hasArg(dataset)) dataset <- attr(data, "dataset")

  dataset <- get_dataset(dataset)


  # Variables

  # No variables defined
  if (!hasArg(variables)) {

    variables <- get_variables(dataset = dataset)

    # Variables defined
  } else {

    # Check if input is vector
    vector.input <- tryCatch(is.atomic(variables),
                             error = function(cond) FALSE)

    # Default variables
    if (vector.input && length(variables) == 1 && variables == "default") {
      variables <- get_variables(variables = "default", dataset = dataset)
    }


    # if variables input is vector
    if (!vector.input) {
      variables <- get_variables(variables = !!dplyr::enquo(variables),
                                 dataset = dataset)
    }

  }

  # Keep all non-variables columns
  columns <- setdiff(colnames(data), pOmics::.data_columns(data_attributes))

  variables <- intersect(variables, colnames(data))

  columns <- c(columns, variables)


  # Remove columns
  data <- dplyr::select(data, dplyr::all_of(columns))

  # Reset attributes
  data <- data %>%
    .set_data_attributes(data_attributes)


  # Output name
  if (!hasArg(output)) output <- input

  # Prepare return
  if (input_list[["list.input"]]) {
    data_[[output]] <- data
    attr(data_, "data") <- output
  }

  else data_ <- data

  # Return
  return(data_)

}
