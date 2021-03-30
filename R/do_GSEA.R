#' Performs gene set enrichment on numeric protein vector with specified Ontology
#'
#' @param proteins numeric score vector
#' @param inverse enrich for terms in higher values/scores
#' @param database database to use
#' @param algorithm algorithm to use ("classic", "elim", "weight", "weight01")
#' @param threshold p-value/confidence threshold to exclude terms
#' @param add.info Add additional information (takes longer)
#'
#' @return
#' @export
#'
#'
do_GSEA <- function(proteins, inverse = F, database, algorithm, threshold, add.info = F) {

  # GO enrichment
  if (database %in% c("CC", "BP", "MF")) results <- do_GSEA_GO(proteins = proteins,
                                                                     inverse = inverse,
                                                                     ontology = database,
                                                                     algorithm = algorithm,
                                                                     threshold = threshold,
                                                                     add.info = add.info)
  #

  # Return results
  return(results)

}
