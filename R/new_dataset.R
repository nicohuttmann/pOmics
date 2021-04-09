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
new_dataset <- function(import, name, data.origin, species, load.UniProt.ws = T, identifier, data.types) {




  initialize_data_structure()






  # Name
  name <- ask_name(name = name, message = "Name of dataset: ", exclude = get_datasets(print = FALSE))

  # Add name
  attr(import, "name") <- name





  # Save whole dataset to .info list


  raw_dataset <- import2raw_dataset(import = import, identifier = identifier)



  ########



  # # Identify data origin
  # # No data origin specified or unknown data origin given
  # if (!hasArg(data.origin) || !is_data_origin(data.origin)) {
  #   #
  #   cat("Identify data origin...\r")
  #   attr(dataset, "data_origin") <- identify_data_origin()
  # # Data origin given and known
  # } else {
  #   #
  #   attr(dataset, "data_origin") <- data.origin
  # }
  #
  # ### Message
  # message(paste0("(", counter, "/n) Data origin: ", attr(dataset, "data_origin")))
  # counter <- counter + 1




  # # Identify separator
  # if (!is.na(attr(dataset, "data_origin"))) {
  #
  #   # Get default
  #   attr(dataset, "separator") <- get_defaults(attr(dataset, "data_origin"), "separator")
  #
  # } else {
  #
  # cat("Identify separator...\r")
  # attr(dataset, "separator") <- identify_separator(import)
  #
  # ### Message
  # message(paste0("(", counter, "/n) Separator: ", attr(dataset, "separator")))
  # counter <- counter + 1
  #
  # }




  ######################




  # # Identify taxonomy Id
  # if (hasArg(species) && species == FALSE) {
  #
  #   load.UniProt.ws <- FALSE
  #
  #
  # } else {
  #
  #   #
  #   cat("Identify species...\r")
  #
  #   # Argument given
  #   if (hasArg(species)) {
  #
  #     #Id or name given
  #     attr(dataset, "taxId") <- get_taxId(species)
  #
  #     # Search for Id
  #   } else {
  #     attr(dataset, "taxId") <- get_taxId(import)
  #   }
  #
  #   # Species name
  #   attr(dataset, "species") <- dataset %>%
  #     attr("taxId") %>%
  #     suppressMessages() %>%
  #     UniProt.ws::lookupUniprotSpeciesFromTaxId()
  #
  #
  #
  #
  #   ### Message 2
  #   message(paste0("(", counter, "/n) Species: ", attr(dataset, "species")))
  #   counter <- counter + 1
  #
  #
  # }






  ##############################




  # # Load UniProt.ws
  # if (load.UniProt.ws) {
  #   cat("Load UniProt.ws...\r")
  #   load_UniProt(attr(dataset, "taxId"))
  #
  #   ### Message
  #   message(paste0("(", counter, "/n) UniProt.ws: loaded"))
  #   counter <- counter + 1
  # }



  #####################








  ########################



  # Update
  #cat("Adding data...\r")


  add_dataset(name = name)


  ###################################


  # Variables
  transfer_variables_data(name, data.types[data.types %in% colnames(raw_dataset[["variables.data"]])])


  # Observations
  transfer_observations(name)




  transfer_data_frames(name, data.types[data.types %in% names(raw_dataset[["data.frames"]])])

  ### Message
  #message(paste0("(", counter, "/n) Observation dataframe setop."))
  #counter <- counter + 1





}
