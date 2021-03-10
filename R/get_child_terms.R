#' Returns all offspring terms of given GO terms
#'
#' @param terms GO terms
#'
#' @return
#' @export
#'
#' 
get_child_terms <- function(terms) {
  
  # Check input
  if (!hasArg(terms)) return(NULL)
  
  # Prepare return vector
  terms.return <- terms
  
  # Get ontology
  term.ontology <- BiocGenerics::Ontology(terms)
  
  # Add child terms
  for (term in terms) {
    
    # CC
    if (term.ontology[term] == "CC") {
      # 
      terms.return <- c(terms.return, GO.db::GOCCOFFSPRING[[term]])
      
    # BP   
    } else if (term.ontology[term] == "BP") {
      # 
      terms.return <- c(terms.return, GO.db::GOBPOFFSPRING[[term]])
      
    # MF  
    } else {
      # 
      terms.return <- c(terms.return, GO.db::GOMFOFFSPRING[[term]])
      
    }
    
  }
  
  return(unique(terms.return))
  
}
