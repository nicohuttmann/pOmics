#' Performs overexpression enrichment on logical protein vector with specified Ontology
#'
#' @param proteins character vector
#' @param database database to use
#' @param algorithm algorithm to use ("classic", "elim", "weight", "weight01")
#'
#' @return
#' @export
#'
#'
do_enrichment_fisher <- function(proteins, database, algorithm) {

  # GO enrichment
  if (database %in% c("CC", "BP", "MF")) results <- enrichment_fisher_GO(proteins = proteins,
                                                                         ontology = database,
                                                                         algorithm = algorithm)
  #

  # Return results
  return(results)

}
