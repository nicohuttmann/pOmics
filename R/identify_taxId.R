#' Checks protein Ids for their species
#'
#' @param x vector of identifiers
#' @param try counter
#' @param silent should message be printed when Id found
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
identify_taxId <- function(x, try = 1, silent = F) {

  if (length(x) >= try) {

    text <- tryCatch(readLines(paste0("https://www.uniprot.org/uniprot/", x[try]), warn = F),
                     error = function(cond) {
                       "a"
                     },
                     warning = function(cond) {
                       "a"
                     }
                     )

    taxId <- text %>%
      regmatches(regexpr("OX=\\d+", text)) %>%
      substring(4) %>%
      as.numeric()

    taxonomy <- tryCatch(
      suppressMessages(UniProt.ws::lookupUniprotSpeciesFromTaxId(taxId)),
      error = function(cond) {
        0
        }
      )


    if (taxonomy == 0) {
      taxId <- identify_taxId(x = x, try = try + 1, silent = F)
    }


    if (taxId != 0) {

      if (!silent && taxonomy != 0) {
        message(paste("Species:", taxonomy))
      }
      # Return tax Id
      taxId

    } else {
      0
    }


  } else {
    0
  }

}
