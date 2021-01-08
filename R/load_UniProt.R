#' Loads the species specific UniProt.ws to the .GlobalEnv
#'
#' @param taxId taxonomy Id
#'
#' @return
#' @export
#'
#'
load_UniProt <- function(taxId = get_taxId()) {

  # Check if data base list exists
  if (!exists(".databases")) {

    assign(".databases",
           list(),
           pos = .GlobalEnv)

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
