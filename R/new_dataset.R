#' Sorts raw data into data list with attributes
#'
#' @param import imported raw data frame
#' @param name name of dataset
#' @param data.origin Identify origin software of data
#' @param species protein origin (T or F or UniProt taxonomy ID)
#' @param load.UniProt.ws (optional) Should UniProt data be loaded
#' @param identifier (optional) specific vector of identifier column/s
#' @param data.types (optional) types of data to extract; uses defaults if not specified
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
new_dataset <- function(import, name, data.origin, species,
                        identifier = "Protein.IDs", data.types = "Peptides") {

  # Create lists for data storage
  initialize_data_structure()


  # Name
  name <- ask_name(name = name,
                   message = "Name of dataset: ",
                   exclude = get_datasets())

  # Add name
  attr(import, "name") <- name



  # Save whole dataset to .info list
  raw_dataset <- import2raw_dataset(import = import, identifier = identifier)


  # Add dataset
  dataset(name = name)

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
