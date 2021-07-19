#' Sorts raw data into data list with attributes
#'
#' @param import imported raw data frame
#' @param name name of dataset
#' @param data.type output type from MaxQuant
#' @param identifier (optional) specific vector of identifier column/s
#' @param data.columns (optional) types of data to extract; uses defaults if not specified
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
new_dataset <- function(import, name, data.type,
                        identifier, data.columns) {

  # Create lists for data storage
  initialize_data_structure()


  # Add name
  if (hasArg(name)) {

    attr(import, "name") <- name

  } else {

    name <- attr(import, "name")

  }


  if (!hasArg(data.type)) {
    data.type <- data.type = attr(import, "data.type")
  }

  default_parameters <-
    MaxQuant_import_defaults(data.type = data.type)


  if (!hasArg(identifier)) {
    identifier <- default_parameters[["identifier"]]
  }

  if (!hasArg(data.columns)) {
    data.columns <- default_parameters[["data.columns"]]
  }

  # Save whole dataset to .info list
  raw_dataset <- import2raw_dataset(import = import, identifier = identifier)


  # Add dataset
  add_dataset(name = name)

  # Variables
  transfer_variables_data(name = name,
                          columns =
                            data.types[data.types %in%
                            colnames(raw_dataset[["variables.data"]])])


  # Observations
  transfer_observations(name)

  # Transfer data frames
  transfer_data_frames(name = name,
                       data.type =
                         data.types[data.types %in%
                                      names(raw_dataset[["data.frames"]])])



}
