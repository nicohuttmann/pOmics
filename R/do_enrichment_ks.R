#' Performs gene set enrichment on numeric protein vector with specified Ontology
#'
#' @param proteins numeric score vector
#' @param database database to use
#'
#' @return
#' @export
#'
#'
do_enrichment_ks <- function(proteins, database) {
  
  # GO enrichment
  if (database %in% c("CC", "BP", "MF")) results <- enrichment_ks_GO(proteins = proteins,
                                                                     ontology = database)
  # 
  
  # Return results
  return(results)
  
}
  