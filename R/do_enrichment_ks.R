#' Performs gene set enrichment on numeric protein vector with specified Ontology
#'
#' @param proteins numeric score vector
#' @param database database to use
#' @param algorithm algorithm to use ("classic", "elim", "weight", "weight01")
#'
#' @return
#' @export
#'
#'
do_enrichment_ks <- function(proteins, database, algorithm) {

  # GO enrichment
  if (database %in% c("CC", "BP", "MF")) results <- enrichment_ks_GO(proteins = proteins,
                                                                     ontology = database,
                                                                     algorithm = algorithm)
  #

  # Return results
  return(results)

}
