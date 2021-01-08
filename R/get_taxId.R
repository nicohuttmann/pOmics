#' Identifies taxonomy Id if possible, ask otherwise
#'
#' @param x data frame to be inspected
#'
#' @return
#' @export
#'
#'
get_taxId <- function(x) {

  # Tries to determine taxId from given data
  if (hasArg(x)) {

    # Prepares entries to test from given data frame
    id.list <- unlist(x[1:ifelse(nrow(x) > 200, 200, nrow(x)), !unlist(lapply(imports[[1]], is.numeric))])
    id.list <- c(id.list,
                 keep.first(id.list))

    taxId <- protein2taxID(id.list, silent = T)



    if (taxId == 0) {
      found <- FALSE
      message("Species could not be determined.")
    } else {
      found <- TRUE
    }

    # Let's user specify the taxId manually
    while(!found) {

      Id2name <- UniProt.ws::availableUniprotSpecies(readline("Enter a species name: "))

      name <- menu(Id2name$`Species name`,
                   title = "Select species:")

      taxId <- Id2name$`taxon ID`[ifelse(name == 0, 9606, name)]

      found <- ifelse(menu(c("Yes", "No"),
                           title = paste0("Correct species? (",
                                          UniProt.ws::lookupUniprotSpeciesFromTaxId(taxId),
                                          ")")) == 1,
                      TRUE,
                      FALSE)

    }

  } else {

    found <- FALSE

    while(!found) {

      Id2name <- UniProt.ws::availableUniprotSpecies(readline("Enter a species name: "))

      name <- menu(Id2name$`Species name`,
                   title = "Select species:")

      taxId <- Id2name$`taxon ID`[ifelse(name == 0, 9606, name)]

      found <- ifelse(menu(c("Yes", "No"),
                           title = paste0("Correct species? (",
                                          UniProt.ws::lookupUniprotSpeciesFromTaxId(taxId),
                                          ")")) == 1,
                      TRUE,
                      FALSE)

    }

  }


  taxId

}
