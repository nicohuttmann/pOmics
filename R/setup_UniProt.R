#' Loads the species specific UniProt.ws to the .GlobalEnv
#'
#' @param taxId taxonomy Id
#' @param dataset dataset
#'
#' @return
#' @export
#'
#'
setup_UniProt <- function(taxId, dataset) {

  # Check if data base list exists
  if (!exists(".databases")) {

    .databases <<- list()

  }


  # Get dataset
  if (hasArg(dataset)) {
    dataset <- get_dataset(dataset)
  }



  # tax ID
  if (hasArg(taxId)) {
    taxId <- get_taxId(taxId)
  } else if (hasArg(dataset)) {
    taxId <- get_taxId(.datasets[[dataset]][["variables"]])
  } else {
    taxId <- get_taxId()
  }


  # Check if .databases contains UniProt list
  if (!"UniProt" %in% names(.databases)) {

    .databases[["UniProt"]] <<- list()

  }



  # Check is taxonomy is already loaded and eventually load
  if (as.character(taxId) %in% names(.databases[["UniProt"]])) {

      message(paste0("UniProt.ws for ",
                     UniProt.ws::lookupUniprotSpeciesFromTaxId(taxId),
                     " already loaded."))
  } else {

    .databases[["UniProt"]][[as.character(taxId)]] <<- UniProt.ws::UniProt.ws(taxId)

  }

}
