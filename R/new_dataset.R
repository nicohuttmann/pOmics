#' Sorts raw data into data list with attributes
#'
#' @param x raw data frame
#' @param name name of dataset
#' @param load.UniProt.ws Should UniProt data be loaded
#' @param return Should dataset be returned?
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
new_dataset <- function(x, name , load.UniProt.ws = T, return = F) {

  # Setup
  # Clear console
  cat("\014")


  counter <- 1
  n <- 100


  ##########



  # Start list
  dataset <- list()


  # Name
  if (!hasArg(name)) name <- as.character(length(.datasets) + 1)

  # Add name
  attr(dataset, "name") <- name




  # Identify separator
  cat("Identify separator...\r")
  attr(dataset, "sep") <- most.common.character(x)
  # Message 1
  message(paste0("(1/n) Identified separator: ", attr(dataset, "sep")))


  ######################


  # Identify taxonomy Id
  cat("Identify species...\r")
  attr(dataset, "taxId") <- get_taxId(x)
  # Species name
  attr(dataset, "taxonomy") <- dataset %>%
    attr("taxId") %>%
    suppressMessages() %>%
    UniProt.ws::lookupUniprotSpeciesFromTaxId()
  # Message 2
  message(paste0("(2/n) Identified species: ", attr(dataset, "taxonomy")))


  ##############################


  # Load UniProt.ws
  if (load.UniProt.ws) {
    cat("Load UniProt.ws...\r")
    load_UniProt(attr(dataset, "taxId"))
    message(paste0("(3/n) UniProt.ws loaded."))
  }




  # Data matrix
  attr(dataset, "data") <- NA

  # Create observations attribute
  attr(dataset, "observations") <- NA


  # Find identifiers
  attr(dataset, "variables.type") <- NA
  attr(dataset, "variables") <- get_identifiers(x, sep = attr(dataset, "sep"))

  # Set row names to identifiers
  suppressWarnings(rownames(x) <- attr(dataset, "variables"))

  # Update
  cat("Start to add data to the list.\r")

  #
  raw.dataset <- x %>%
    df2groupedList() %>%
    sep_vector2list(sep = attr(dataset, "sep")) %>%
    list_entries2vector()


  dataset[["data"]] <- list()


  # Add data to dataset
  # Quantitative value; mostly LFQ
  dataset[["data"]][["LFQ"]] <- raw.dataset[[findORchoose(names = names(raw.dataset)[unlist(lapply(raw.dataset, is.matrix))],
                                               patterns = c("intensity", "LFQ", "int"),
                                               title = "Quantitative data?")]]
  # Add entry to attribute data
  attr(dataset, "data") <- if (is.na(attr(dataset, "data"))) "LFQ" else c(attr(dataset, "data"), "LFQ")

  # Identification
  dataset[["data"]][["Identification"]] <- raw.dataset[[findORchoose(names = names(raw.dataset)[unlist(lapply(raw.dataset, is.matrix))],
                                                          patterns = c("Identification", "identified", "id"),
                                                          title = "Identification data?")]]
  # Add entry to attribute data
  attr(dataset, "data") <- if (is.na(attr(dataset, "data"))) "Identification" else c(attr(dataset, "data"), "Identification")



  message(paste0("(4/n) Added: ", paste(attr(dataset, "data"), collapse = ", ")))


  ################################


  cat("Collect variables...\r")


  # Build and add Names dataframe
  variables <- tibble::tibble(variables = attr(dataset, "variables"))
                              # UNIPROTKB = NA,
                              # GENES = NA,
                              # `PROTEIN-NAMES` = NA,
                              # ENTREZ_GENE = NA)


  # Get column name for data with findORchoose; if not found fill with NAs
  # Protein accession id
  prot <- raw.dataset[[findORchoose(names = names(raw.dataset)[unlist(lapply(raw.dataset, function(x) !is.matrix(x) && is.character(x)))],
                                 patterns = c("protein", "accession", "id", "Uniprot"),
                                 title = "Protein accession Ids?")]]
  if (!is.null(prot)) variables <- tibble::add_column(variables, UNIPROTKB = prot)
  else variables <- dplyr::mutate(variables, UNIPROTKB = NA_character_)


  # Gene symbol
  gen <- raw.dataset[[findORchoose(names = names(raw.dataset)[unlist(lapply(raw.dataset, function(x) !is.matrix(x) && is.character(x)))],
                                                 patterns = c("gene", "symbol"),
                                                 title = "Gene symbols?")]]
  if (!is.null(gen)) variables <- tibble::add_column(variables, GENES = gen)
  else variables <- dplyr::mutate(variables, GENES = NA_character_)

  # Protein name
  nam <- raw.dataset[[findORchoose(names = names(raw.dataset)[unlist(lapply(raw.dataset, function(x) !is.matrix(x) && is.character(x)))],
                                                 patterns = c("Protein", "name"),
                                                 title = "Protein names?")]]
  if (!is.null(nam)) variables <- tibble::add_column(variables, `PROTEIN-NAMES` = nam)
  else variables <- dplyr::mutate(variables, `PROTEIN-NAMES` = NA_character_)

  # Entrez gene id
  eg <- raw.dataset[[findORchoose(names = names(raw.dataset)[unlist(lapply(raw.dataset, function(x) !is.matrix(x) && is.character(x)))],
                                                   patterns = c("Entrez", "EG"),
                                                   title = "Entrez gene Id?")]]
  if (!is.null(eg)) variables <- tibble::add_column(variables, ENTREZ_GENE = eg)
  else variables <- dplyr::mutate(variables, ENTREZ_GENE = NA_character_)




  # Determine identifier type by comparing columns with rownames/identifiers
  for (i in 2:ncol(variables)) {
    if (identical(dplyr::pull(variables, 1), unname(dplyr::pull(variables, i)))) {
      attr(dataset, "variables.type") <- colnames(variables)[i]
      break
    }
  }


message(paste0("(5/n) Identifier type: ", attr(dataset, "variables.type")))


  #########################



  # Complete variables data frame if missing values are present
  # AND if UniProt database is available
  if(check.database("UniProt", taxId = attr(dataset, "taxId"))) {

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
                               keytype = attr(dataset, "variables.type"))
        # Add missing data
        for (j in dplyr::pull(temp, 1)) {
          variables[match(j, dplyr::pull(variables, 1)), i] <- temp[j, i]
        }
        message(paste0("  Finished ", i))

      }

    }

  }

  # Message
  message("(6/n) Variables completed.")


  # Add Names data frame
  dataset[["variables"]] <- variables


  # Add variable groups template
  dataset[["variable_groups"]] <- variables %>% dplyr::select(1)


  ###################################


  # Observations
  # Assumes for now that all observations in all data are equal
  attr(dataset, "observations") <- rownames(dataset[[attr(dataset, "data")[1]]])

  # Build observation matrix to store names and group info
  observations <- tibble::tibble(original.names = attr(dataset, "observations"),
                                 adj.names = attr(dataset, "observations"))



  # Add observations data frame
  dataset[["observations"]] <- observations


  # Add observation groups template
  dataset[["observation_groups"]] <- observations %>%
    dplyr::select(1) %>%
    dplyr::rename("observations" = "original.names")


  # Add new dataset
  add_dataset(dataset)


  # Message
  message("(n/n) New dataset saved.")
  Sys.sleep(1)



  # View data
  view_data()

  # Return
  if (return) dataset

}
