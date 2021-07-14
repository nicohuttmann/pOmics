#' Retrieve annotation data from imported fasta files
#'
#' @param keys vector of protein identifiers (see keytypes_org())
#' @param columns data columns to return(see columns_org())
#' @param output format of output ("vector.keep" (default), "vector.na, "vector.rm", "mapping.na", "mapping.rm", "TERM2GENE")
#' @param keytype Id type of supplied keys (identified automatically if not provided)
#' @param dataset dataset
#'
#' @return
#' @export
#'
#'
select_fasta <- function(keys, columns, output = "vector.keep", keytype, dataset) {

  # Keys given
  if (!hasArg(keys)) {

    cat("No keys given.")
    return(invisible(NULL))

  }

  # Get dataset
  dataset <- get_dataset(dataset)

  taxId <- get_dataset_attr(which = "taxId", dataset = dataset)

  if (is.null(taxId)) {

    message("No taxonomy Id available. Use setup_taxonomy_information().")

    return(keys)

  }

  # Check database
  if (!check_database(id = taxId, type = "fasta_information")) {

    message(paste0("No fasta information found for taxId: ", taxId))

    return(keys)

  }



  fasta.database <- get_database(id = taxId, type = "fasta_information")

  # Test colnames and keys
  if (!hasArg(keytype) || length(keytype) > 1 || !keytype %in% colnames(fasta.database)) {


    keytype <- 1
    keytypes <- c("UNIPROT", "SYMBOL", "GENENAME", colnames(fasta.database))

    while (is.numeric(keytype)) {

      if (any(keys %in% fasta.database[[keytypes[keytype]]]))
        keytype <- keytypes[keytype]

      else
        keytype <- keytype + 1


    }

    # Test keys
  } else {

    if (all(!keys %in% fasta.database[[keytypes[keytype]]])) {

      message("No keys match the database. Change the keytype argument or remove it.")

      return(keys)

    }

  }


  # Extract data
  output.data <- fasta.database %>%
    dplyr::select(c(keytype, columns)) %>%
    dplyr::filter(.data[[keytype]] %in% keys)


  # Format output
  output.data <- select2output(output.data = output.data,
                               keys = keys,
                               output = output,
                               dataset = dataset)


  # Return
  return(output.data)

}
