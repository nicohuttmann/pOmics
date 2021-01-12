#' Sorts raw data into data list with attributes
#'
#' @param x raw data frame
#' @param name name of dataset
#' @param identify.data.origin Identify origin software of data
#' @param load.UniProt.ws Should UniProt data be loaded
#' @param return Should dataset be returned?
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
new_dataset <- function(x, name , identify.data.origin = F, load.UniProt.ws = T, return = F) {

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

  attr(dataset, "variables") <- NA
  attr(dataset, "variable_type") <- NA
  attr(dataset, "default_variables") <- NA
  attr(dataset, "default_variables_names") <- NA

  attr(dataset, "observations") <- NA
  attr(dataset, "observations_sets") <- NA
  attr(dataset, "default_observations_set") <- NA
  attr(dataset, "default_observations") <- NA
  attr(dataset, "default_observations_names") <- NA

  attr(dataset, "data_types") <- NA
  attr(dataset, "default_data_type") <- "LFQ"
  attr(dataset, "LFQ") <- NA
  attr(dataset, "default_LFQ") <- NA
  attr(dataset, "Identification") <- NA
  attr(dataset, "default_Identification") <- NA



  # Name
  if (!hasArg(name)) name <- paste0("dataset", as.character(length(.datasets) + 1))

  # Add name
  attr(dataset, "name") <- name

  ### Message
  message(paste0("(", counter, "/n) Name: ", attr(dataset, "name")))
  counter <- counter + 1


  # Identify data origin
  if (identify.data.origin) {

    #
    cat("Identify data origin...\r")
    attr(dataset, "data_origin") <- identify_data_origin()

    ### Message
    message(paste0("(", counter, "/n) Data origin: ", attr(dataset, "data_origin")))
    counter <- counter + 1
  }



  # Identify separator
  if (!is.na(attr(dataset, "data_origin"))) {

    # Get default
    attr(dataset, "separator") <- get_defaults(attr(dataset, "data_origin"), "separator")

  } else {

  cat("Identify separator...\r")
  attr(dataset, "separator") <- most_common_character(x)

  ### Message
  message(paste0("(", counter, "/n) Separator: ", attr(dataset, "separator")))
  counter <- counter + 1
  }




  ######################




  # Identify taxonomy Id
  cat("Identify species...\r")
  attr(dataset, "taxId") <- get_taxId(x)
  # Species name
  attr(dataset, "species") <- dataset %>%
    attr("taxId") %>%
    suppressMessages() %>%
    UniProt.ws::lookupUniprotSpeciesFromTaxId()
  ### Message 2
  message(paste0("(", counter, "/n) Species: ", attr(dataset, "species")))
  counter <- counter + 1

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
  attr(dataset, "variables") <- get_identifiers(x, sep = attr(dataset, "separator"))

  # Set row names to identifiers
  suppressWarnings(rownames(x) <- attr(dataset, "variables"))




  ########################



  # Update
  cat("Adding data to the dataset...\r")

  # Aggregate data frame
  raw.dataset <- x %>%
    df2groupedList() %>%
    sep_vector2list(sep = attr(dataset, "separator")) %>%
    list_entries2vector()


  # Quantitative value; mostly LFQ
  dataset[["LFQ"]] <- tibble::lst()
  dataset[["LFQ"]][["raw"]] <- raw.dataset[[findORchoose(names = names(raw.dataset)[unlist(lapply(raw.dataset, is.matrix))],
                                               patterns = c("intensity", "LFQ", "int"),
                                               title = "Quantitative data?")]]
  # Add entry to attribute data
  attr(dataset, "LFQ") <- "raw"

  # Set default LFQ dataset
  attr(dataset, "default_LFQ") <- "raw"

  # Update data_type attribute
  attr(dataset, "data_types") <- if (is.na(attr(dataset, "data_types"))) "LFQ" else c(attr(dataset, "data_types"), "LFQ")




  # Identification
  dataset[["Identification"]] <- tibble::lst()
  dataset[["Identification"]][["raw"]] <- raw.dataset[[findORchoose(names = names(raw.dataset)[unlist(lapply(raw.dataset, is.matrix))],
                                                          patterns = c("Identification", "identified", "id"),
                                                          title = "Identification data?")]]
  # Add entry to attribute data
  attr(dataset, "Identification") <- "raw"

  # Set default Identification dataset
  attr(dataset, "default_Identification") <- "raw"

  # Update data_type attribute
  attr(dataset, "data_types") <- if (is.na(attr(dataset, "data_types"))) "Identification" else c(attr(dataset, "data_types"), "Identification")


  ### Message
  message(paste0("(", counter, "/n) Added data: ", paste(attr(dataset, "data_types"), collapse = ", ")))
  counter <- counter + 1




  ################################




  cat("Identify variable names...\r")


  # Build and add Names dataframe
  variables <- tibble::tibble(variables = attr(dataset, "variables"))
                              # UNIPROTKB = NA,
                              # GENES = NA,
                              # `PROTEIN-NAMES` = NA,
                              # ENTREZ_GENE = NA)


  # Get column name for data with findORchoose; if not found fill with NAs
  # Protein accession id
  prot <- raw.dataset[[find_data_entry(data = raw.dataset, entry = "UNIPROTKB", data.origin = attr(dataset, "data_origin"))]]
  if (!is.null(prot)) variables <- tibble::add_column(variables, UNIPROTKB = prot)
  else variables <- dplyr::mutate(variables, UNIPROTKB = NA_character_)


  # Gene symbol
  gen <- raw.dataset[[find_data_entry(data = raw.dataset, entry = "GENES", data.origin = attr(dataset, "data_origin"))]]
  if (!is.null(gen)) variables <- tibble::add_column(variables, GENES = gen)
  else variables <- dplyr::mutate(variables, GENES = NA_character_)

  # Protein name
  nam <- raw.dataset[[find_data_entry(data = raw.dataset, entry = "PROTEIN-NAMES", data.origin = attr(dataset, "data_origin"))]]
  if (!is.null(nam)) variables <- tibble::add_column(variables, `PROTEIN-NAMES` = nam)
  else variables <- dplyr::mutate(variables, `PROTEIN-NAMES` = NA_character_)

  # Entrez gene id
  eg <- raw.dataset[[find_data_entry(data = raw.dataset, entry = "ENTREZ_GENE", data.origin = attr(dataset, "data_origin"))]]
  if (!is.null(eg)) variables <- tibble::add_column(variables, ENTREZ_GENE = eg)
  else variables <- dplyr::mutate(variables, ENTREZ_GENE = NA_character_)




  # Determine identifier type by comparing columns with rownames/identifiers
  if (!is.na(attr(dataset, "data_origin"))) {
    #
    if (!is.na(get_defaults(attr(dataset, "data_origin"), type = "variables_type"))) {
      #
      attr(dataset, "variables_type") <- get_defaults(attr(dataset, "data_origin"), type = "variables_type")
    } else {
      # Identify manually
      for (i in 2:ncol(variables)) {
        if (identical(dplyr::pull(variables, 1), unname(dplyr::pull(variables, i)))) {
          attr(dataset, "variables_type") <- colnames(variables)[i]
          break
        }
      }
    }

    # No data origin
  } else {
    # Idenify manually
    for (i in 2:ncol(variables)) {
      if (identical(dplyr::pull(variables, 1), unname(dplyr::pull(variables, i)))) {
        attr(dataset, "variables_type") <- colnames(variables)[i]
        break
      }
    }
  }



  ### Message
  message(paste0("(", counter, "/n) Identifier type: ", attr(dataset, "variables_type")))
  counter <- counter + 1




  #########################




  # Complete variables data frame if missing values are present
  # AND if UniProt database is available
  if(check_database("UniProt", taxId = attr(dataset, "taxId"))) {

    # Complete
    for (i in colnames(variables)) {
      # Check is any data is missing
      if (anyNA(variables[, i])) {

        # Message
        cat(paste0("Complete ", i, " \r"))
        # Query missing data
        temp <- select_UniProt(x = .databases[["UniProt"]][[as.character(attr(dataset, "taxId"))]],
                               keys = names(which(is.na(dplyr::pull(variables, i, name = variables)))),
                               columns = i,
                               keytype = attr(dataset, "variables_type"))
        # Add missing data
        for (j in dplyr::pull(temp, 1)) {
          variables[match(j, dplyr::pull(variables, 1)), i] <- temp[j, i]
        }
        message(paste0("  Finished ", i))

      }

    }

  }




  ### Message
  #message(paste0("(", counter, "/n)  Variables completed."))
  #counter <- counter + 1


  # Add Names data frame
  dataset[["variables"]] <- variables %>% dplyr::mutate(all = TRUE)

  # Set attributes
  attr(dataset, "default_variables") <- "all"
  attr(dataset, "default_names_variables") <- "GENES"


  ### Message
  message(paste0("(", counter, "/n)  Setup variable dataframes."))
  counter <- counter + 1




  ###################################




  # Observations
  # Assumes for now that all observations in all data are equal
  attr(dataset, "observations") <- rownames(dataset[[attr(dataset, "data_types")[1]]][["raw"]])

  # Build observation matrix to store names and group info
  observations <- tibble::tibble(observations = attr(dataset, "observations"))


  # Add observations data frame
  dataset[["observations"]] <- tibble::lst()
  dataset[["observations"]][["raw"]] <- observations %>% dplyr::mutate(all = TRUE)


  # Add attributes
  attr(dataset, "observations_sets") <- "raw"
  attr(dataset, "default_observations_set") <- "raw"
  attr(dataset, "default_observations") <- "all"
  attr(dataset, "default_observations_names") <- "observations"

  ### Message
  message(paste0("(", counter, "/n)  Setup observation dataframes."))
  counter <- counter + 1




  #######################




  # Add new dataset
  if (add_dataset(dataset)) {
    ### Message
    message(paste0("(", counter, "/", counter, ")  Dataset saved."))
  } else {
    stop("Something went wrong while saving the dataset.")
  }



  # View data
  view_data()


  # Return
  if (return) dataset

}
