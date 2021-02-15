#' Sorts raw data into data list with attributes
#'
#' @param x raw data frame
#' @param name name of dataset
#' @param data.origin Identify origin software of data
#' @param species protein origin (T or F or UniProt taxosnomy ID)
#' @param load.UniProt.ws (optional) Should UniProt data be loaded
#' @param return Should dataset be returned?
#' @param identifier (optional) specific vector of identifier column/s
#' @param data.types (optional) types of data to extract; uses defaults if not specified
#' @param min.similarity minimum similarity oi column names
#' @param min.groupsize minimum number of samples
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
new_dataset <- function(x, name, data.origin, species, load.UniProt.ws = T, return = F, identifier, data.types, min.similarity = 8, min.groupsize = 6) {

  # Setup
  # Clear console
  cat("\014")


  counter <- 1
  n <- 100


  ##########

  # Start list
  dataset <- tibble::lst()


  # Preset attributes
  attr(dataset, "name") <- NA
  attr(dataset, "data_origin") <- NA
  attr(dataset, "separator") <- NA

  attr(dataset, "taxId") <- NA
  attr(dataset, "species") <- NA


  attr(dataset, "default_variables") <- "All"
  attr(dataset, "default_variables_labels") <- "variables"

  attr(dataset, "default_observations_set") <- "raw"
  attr(dataset, "default_observations") <- "All"
  attr(dataset, "default_labels") <- "observations"
  attr(dataset, "default_groups") <- NA



  # Name
  if (!hasArg(name)) name <- paste0("dataset", as.character(length(.datasets) + 1))

  # Add name
  attr(dataset, "name") <- name

  ### Message
  message(paste0("(", counter, "/n) Name: ", attr(dataset, "name")))
  counter <- counter + 1


  ########



  # Identify data origin
  # No data origin specified or unknown data origin given
  if (!hasArg(data.origin) || !is_data_origin(data.origin)) {
    #
    cat("Identify data origin...\r")
    attr(dataset, "data_origin") <- identify_data_origin()
  # Data origin given and known
  } else {
    #
    attr(dataset, "data_origin") <- data.origin
  }

  ### Message
  message(paste0("(", counter, "/n) Data origin: ", attr(dataset, "data_origin")))
  counter <- counter + 1




  # Identify separator
  if (!is.na(attr(dataset, "data_origin"))) {

    # Get default
    attr(dataset, "separator") <- get_defaults(attr(dataset, "data_origin"), "separator")

  } else {

  cat("Identify separator...\r")
  attr(dataset, "separator") <- identify_separator(x)

  ### Message
  message(paste0("(", counter, "/n) Separator: ", attr(dataset, "separator")))
  counter <- counter + 1

  }




  ######################




  # Identify taxonomy Id
  if (hasArg(species) && species == FALSE) {

    load.UniProt.ws <- FALSE


  } else {

    #
    cat("Identify species...\r")

    # Argument given
    if (hasArg(species)) {

      #Id or name given
      attr(dataset, "taxId") <- get_taxId(species)

      # Search for Id
    } else {
      attr(dataset, "taxId") <- get_taxId(x)
    }

    # Species name
    attr(dataset, "species") <- dataset %>%
      attr("taxId") %>%
      suppressMessages() %>%
      UniProt.ws::lookupUniprotSpeciesFromTaxId()




    ### Message 2
    message(paste0("(", counter, "/n) Species: ", attr(dataset, "species")))
    counter <- counter + 1


  }






  ##############################




  # Load UniProt.ws
  if (load.UniProt.ws) {
    cat("Load UniProt.ws...\r")
    load_UniProt(attr(dataset, "taxId"))

    ### Message
    message(paste0("(", counter, "/n) UniProt.ws: loaded"))
    counter <- counter + 1
  }



  #####################




  # Find identifiers
  variables <- get_identifiers(x, sep = attr(dataset, "separator")) # Removed identifier = identifier

  # Set row names to identifiers
  suppressWarnings(rownames(x) <- variables)




  ########################



  # Update
  cat("Adding data...\r")

  dataset <- add_data2dataset(x = x,
                              dataset = dataset,
                              data.types = data.types,
                              data.origin = attr(dataset, "data_origin"),
                              min.similarity = min.similarity,
                              min.groupsize = min.groupsize)

  ### Message
  message(paste0("(", counter, "/n) Data added."))
  counter <- counter + 1


  ### Message
  message(paste0("(", counter, "/n) Variables dataframe setup."))
  counter <- counter + 1




  ###################################




  # Observations
  # Assumes for now that all observations in all data are equal
  if (length(dataset) > 2) {

  # Build observation matrix to store names and group info
  observations <- tibble::tibble(observations = rownames(dataset[[3]][[1]]))


  # Add observations data frame
  dataset[["observations"]] <- tibble::lst()
  dataset[["observations"]][["raw"]] <- observations %>% dplyr::mutate(All = TRUE)



  ### Message
  message(paste0("(", counter, "/n) Observation dataframe setop."))
  counter <- counter + 1

  }





  #######################




  # Add new dataset
  if (add_dataset(dataset)) {
    ### Message
    message(paste0("(", counter, "/", counter, ") Dataset saved."))
  } else {
    if (menu(choices = c("Yes", "No"), title = "Replace dataset? ") == 1) {
      .datasets[[attr(dataset, "name")]] <<- NULL
      if (add_dataset(dataset)) {
        ### Message
        message(paste0("(", counter, "/", counter, ")  Dataset saved."))
      } else {
        stop("Unclear error. Check name of the dataset.")
      }
    }
  }



  # View data
  view_data()


  # Return
  if (return) dataset

}
