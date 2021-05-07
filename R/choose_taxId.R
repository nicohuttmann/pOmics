#' Helps to find taxonomy Id
#'
#' @return
#' @export
#'
#' 
choose_taxId <- function() {
  
  # 
  found <- FALSE

  while(!found) {
    
    Id2name <- suppressMessages(UniProt.ws::availableUniprotSpecies(readline("Enter a species name: ")))
    
    name <- menu(Id2name$`Species name`,
                 title = "Select species:")
    
    if (name != 0) {
      
      taxId <- Id2name$`taxon ID`[name]
      
      found <- ifelse(menu(c("Yes", "No"),
                           title = paste0("Correct species? (",
                                          suppressMessages(UniProt.ws::lookupUniprotSpeciesFromTaxId(taxId)),
                                          ")")) == 1,
                      TRUE,
                      FALSE)
      
    }
    
    
  }
  
  return(taxId)
  
}
