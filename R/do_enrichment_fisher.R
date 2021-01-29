#' Performs overexpression enrichment on logical protein vector with specified Ontology
#'
#' @param proteins character vector
#' @param database database to use
#'
#' @return
#' @export
#'
#' 
do_enrichment_fisher <- function(proteins, database) {
  
  # GO enrichment
  if (database %in% c("CC", "BP", "MF")) results <- enrichment_fisher_GO(proteins = proteins,
                                                                         ontology = database)
  # 
  
  # Return results
  return(results)
  
}
