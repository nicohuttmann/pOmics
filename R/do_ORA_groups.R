#' Performs overexpression enrichment on protein group vectors
#'
#' @param proteins character vector
#' @param database database to use
#' @param pvalueCutoff p-value/confidence threshold to exclude terms
#' @param pAdjustMethod one of "none", "BH" (Benjamini-Hochberg correction),
#' "hochberg", "bonferroni", "holm", "hommel", "BY", "fdr"
#' @param qvalueCutoff q-value cutoff for annotations
#' @param minGSSize minimum number of annotated proteins to be included
#' @param maxGSSize maximum number of annotated proteins to be included
#' @param dataset dataset
#'
#' @return
#' @export
#'
#'
do_ORA_groups <- function(proteins,
                          database,
                          pvalueCutoff = 0.05,
                          pAdjustMethod = "none",
                          qvalueCutoff = 0.2,
                          minGSSize = 10,
                          maxGSSize = 500,
                          dataset) {

  list.enrichment <- tibble::lst()

  # Check input
  for (group in unique(proteins)) {

    #
    list.enrichment[[as.character(group)]] <-
      do_ORA(proteins = ifelse(proteins == group, 1, 0),
             database = database,
             pvalueCutoff = pvalueCutoff,
             pAdjustMethod = pAdjustMethod,
             qvalueCutoff = qvalueCutoff,
             minGSSize = minGSSize,
             maxGSSize = maxGSSize,
             dataset = dataset)
  }

  # Return
  return(list.enrichment)

}
