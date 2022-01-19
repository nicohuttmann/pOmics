#' Identifies taxonomy Id if possible, ask otherwise
#'
#' @param x data frame to be inspected
#'
#' @return
#' @export
#'
#'
get_taxId <- function(x) {

  #
  found <- FALSE

  # Tries to determine taxId from given data
  if (hasArg(x)) {

    # Test data type; data frame
    if (is.data.frame(x)) {

      # Prepares entries to test from given data frame
      id.list <- unlist(x[1:ifelse(nrow(x) > 200, 200, nrow(x)), !unlist(lapply(x, is.numeric))])
      id.list <- c(id.list,
                   strsplit_keep_first(id.list))

      taxId <- identify_taxId(id.list, silent = T)



      if (taxId == 0) {
        message("Species could not be determined from data frame entries.")
      } else {
        found <- TRUE
      }

    } else if (is.numeric(x)) {


      taxonomy <- tryCatch(
        UniProt.ws::lookupUniprotSpeciesFromTaxId(x),
        error = function(cond) {
          0
        }
      )

      # Test if id was successfully identified
      if (taxonomy > 0) {
        #
        found <- TRUE
        taxId <- x

      }

    } else if(is.character(x)) {

        #
        dummy <- UniProt.ws::availableUniprotSpecies(x)

        #
        if (any(dummy$`Species name` == x)) {

          #
          taxId <- dummy[which(dummy$`Species name` == x), 1]
          #
          found <- TRUE
        }

    }

  }



  # If not found from input
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



  return(taxId)

}
