#' Returns all proteome accession numbers of given sepcies
#'
#' @param id id of proteome
#' @param dataset dataset
#'
#' @return
#' @export
#'
#'
get_proteome <- function(id, dataset) {

  #
  if (!hasArg(id)) {

    dataset <- get_dataset(dataset)

    taxId <- get_dataset_attr(which = "taxId", dataset)

  }

  # Check taxId
  if (is.null(id)) {

    message("No taxId saved in dataset. Use setup_annotations() or a protoeme name.")
    return(invisible(NULL))

  }


  id <- lapply(.databases[["Proteome"]],
               function(x) taxId %in% attr(x = x, which = "taxIds")) %>%
    unlist() %>%
    which_names()


  if (length(id) == 0) {



  } else if (length(id) > 1) {



  }


  if (!check_database(id = id, type = "Proteome")) {

    message(paste0("No proteome database setup for taxId ",
                   taxId, ". Use import_fasta()."))
    return(invisible(NULL))

  }

  # Return
  return(get_database(id = id, type = "Proteome"))

}
