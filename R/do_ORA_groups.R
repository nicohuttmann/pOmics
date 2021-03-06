#' Performs overexpression enrichment on protein group vectors
#'
#' @param proteins character vector
#' @param database database to use
#' @param pvalueCutoff p-value/confidence threshold to exclude terms
#' @param pAdjustMethod one of "none", "BH" (Benjamini-Hochberg correction), "hochberg", "bonferroni", "holm", "hommel", "BY", "fdr"
#' @param qvalueCutoff q-value cutoff for annotations
#' @param minGSSize minimum number of proteins for annotation to be used for enrichment
#' @param maxGSSize maximum number of proteins for annotation to be used for enrichment
#' @param algorithm algorithm to use ("classic", "elim", "weight", "weight01")
#' @param dataset dataset
#'
#' @return
#' @export
#'
#'
do_ORA_groups <- function(proteins, database, pvalueCutoff = 0.05, pAdjustMethod = "none", qvalueCutoff = 0.2, minGSSize = 10,
                          maxGSSize = 500, algorithm = "classic", dataset) {

  list.enrichment <- tibble::lst()

  # Check input
  for (group in unique(proteins)) {

    #
    list.enrichment[[as.character(group)]] <- do_ORA(proteins = ifelse(proteins == group, 1, 0),
                                                     database = database,
                                                     pvalueCutoff = pvalueCutoff,
                                                     pAdjustMethod = pAdjustMethod,
                                                     minGSSize = minGSSize,
                                                     maxGSSize = maxGSSize,
                                                     algorithm = algorithm,
                                                     dataset = dataset)
  }

  # Return
  return(list.enrichment)

}
