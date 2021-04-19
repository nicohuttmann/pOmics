#' Opens UniProt page of given protein
#'
#' @param proteins vector of protein accession Ids
#' @param limit limit of proteins to be searched
#'
#' @return
#' @export
#'
#'
browse_proteins <- function(proteins = "Q63HQ2", limit = 10) {

  # Don't do it
  if (length(proteins) < 100) {

    for (protein in proteins) {
      browseURL(paste0("https://www.uniprot.org/uniprot/", protein))
    }

  } else {

    # Checks lenght of protein vector
    proteins <- proteins[1:ifelse(length(proteins) > limit, limit, length(proteins))]

    # Can given proteins be controlled by the UniProt.ws
    if (!exists(".UniProt")) {

      message(".Uniprot not setup. Function restricted to single proteins.")
      browseURL(paste0("https://www.uniprot.org/uniprot/", proteins[1]))

    } else {

      # Iterates through all proteins
      for (protein in proteins) {

        if (protein %in% attr(.UniProt, "taxIdUniprots")) {
          browseURL(paste0("https://www.uniprot.org/uniprot/", protein))
        } else {
          browseURL(paste0("https://www.uniprot.org/uniprot/", getProtein(protein)))
        }
      }
    }
  }
}
