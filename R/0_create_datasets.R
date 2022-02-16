#' Sorts raw data into data list with attributes
#'
#' @param import imported raw data frame
#' @param name name of dataset
#' @param data.type output type from MaxQuant
#' @param identifier (optional) specific vector of identifier column/s
#' @param modify.identifiers action to perform on identifier column ("not" for
#' nothing; "split" to separate strings by separator)
#' @param data.columns (optional) types of data to extract; uses defaults if
#' not specified
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
new_dataset <- function(import,
                        name,
                        data.type,
                        identifier,
                        modify.identifiers = "not",
                        data.columns) {

  # Create lists for data storage
  initialize_data_structure()


  # Add name
  if (hasArg(name)) {

    attr(import, "name") <- name

  } else {

    name <- attr(import, "name")

  }


  if (!hasArg(data.type)) {
    data.type <- attr(import, "data.type")
  }

  # Get default parameters for file type
  default_parameters <-
    get_MaxQuant_defaults(data.type = data.type)


  #
  if (!hasArg(identifier)) {
    identifier <- default_parameters[["identifier"]]
  }

  if (!hasArg(data.columns)) {
    data.columns <- default_parameters[["data.columns"]]
  }

  # Save whole dataset to .info list
  raw_dataset <- import2raw_dataset(import = import,
                                    identifier = identifier,
                                    modify.identifiers = modify.identifiers)


  # Add dataset
  add_dataset(name = name)

  # Variables
  transfer_variables_data(dataset = name,
                          data.columns =
                            data.columns[data.columns %in%
                                           colnames(raw_dataset[["variables.data"]])])


  # Observations
  transfer_observations(dataset = name)

  # Transfer data frames
  transfer_data_frames(dataset = name,
                       data.columns =
                         data.columns[data.columns %in%
                                        names(raw_dataset[["data.frames"]])])

}


#' Extracts variables and grouped data from imported data frame
#'
#' @param import imported data frame
#' @param identifier columns to use as identifiers
#' @param modify.identifiers action to perform on identifier column ("not" for
#' nothing; "split" to separate strings by separator)
#' @param add add raw_dataset to .info list
#'
#' @return
#' @export
#'
#'
import2raw_dataset <- function(import,
                               identifier,
                               modify.identifiers = "split",
                               add = T) {

  #
  sep <- identify_separator(x = import)

  identifiers <- identify_variables(x = import,
                                    identifier = identifier,
                                    modify.identifiers = modify.identifiers,
                                    sep = sep)

  # Define identifiers
  import <- import %>%
    dplyr::mutate(variables = identifiers, .before = 1)

  # Build raw dataset
  raw_dataset <- import2grouped_data(import = import)

  attr(x = raw_dataset, which = "name") <- attr(import, "name")
  attr(x = raw_dataset, which = "project") <- attr(import, "project")
  attr(x = raw_dataset, which = "data.type") <- attr(import, "data.type")
  attr(x = raw_dataset, which = "path") <- attr(import, "path")
  attr(x = raw_dataset, which = "time") <- attr(import, "time")


  # Set separator
  attr(x = raw_dataset, which = "separator") <- sep




  # Store
  if (add)
    add_raw_dataset(raw_dataset = raw_dataset, name = attr(import, "name"))

  return(raw_dataset)

}


#' Stores data frame in a list and groups adjacent columns
#'
#' @param import imported data frame
#'
#' @return
#' @export
#'
#'
import2grouped_data <- function(import) {

  # List to be filled with grouped columns
  grouped_data <- tibble::lst(variables.data =
                                tibble::tibble(.rows = nrow(import)),
                              data.frames = tibble::lst())


  observations <- identify_observations(import)

  column.names <- gsub(paste(observations, collapse = "|"), "",
                       x = colnames(import))



  # # TRUE means grouped column
  # column.type <- column.names %in% which_names(table(column.names) ==
  # max(table(column.names)))

  # Add data to list
  for (i in which(!duplicated(column.names))) {

    # variables data
    if (sum(column.names[i] == column.names) == 1) {

      grouped_data[["variables.data"]] <- grouped_data[["variables.data"]] %>%
        dplyr::mutate(!!column.names[i] :=
                        dplyr::pull(import, which(column.names[i] ==
                                                    column.names)))

      # Data frames
    } else {


      #
      grouped_data[["data.frames"]][[column.names[i]]] <-
        dplyr::select(import, which(column.names[i] == column.names))

      colnames(grouped_data[["data.frames"]][[column.names[i]]]) <-
        gsub(pattern = column.names[i],
             replacement = "",
             x = colnames(grouped_data[["data.frames"]][[column.names[i]]]))

      names(grouped_data[["data.frames"]])[length(
        grouped_data[["data.frames"]])] <-
        ifelse(substring(text = column.names[i],
                         first = nchar(column.names[i])) == ".",
               substring(text = column.names[i], first = 1,
                         nchar(column.names[i]) - 1),
               column.names[i])
    }

  }


  # Return list
  return(grouped_data)

}


#' Saves raw data frames to .info list
#'
#' @param raw_dataset raw data frame
#' @param name name of dataset
#'
#' @return
#' @export
#'
#'
add_raw_dataset <- function(raw_dataset, name) {

  # Check input
  if (!hasArg(raw_dataset) || !hasArg(name)) stop("Raw data and name must be given.")

  # Check if dataset with same name already exists
  if (is_dataset(name) || is_raw_dataset(name)) {

    stop("Name already exists.")

  } else {

    .info[["raw_datasets"]][[name]] <<- raw_dataset

  }

}
