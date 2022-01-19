#' Performs over-representation analysis using Gene Ontology annotations
#'
#' @param proteins numeric/logical vector of proteins indicating group
#' @param database TERM2GENE data frame
#' @param pvalueCutoff p-value cutoff for annotations
#' @param pAdjustMethod one of "none", "BH" (Benjamini-Hochberg correction),
#' "hochberg", "bonferroni", "holm", "hommel", "BY", "fdr"
#' @param qvalueCutoff q-value cutoff for annotations
#' @param minGSSize minimum number of annotated proteins to be included
#' @param maxGSSize maximum number of annotated proteins to be included
#'
#' @return
#' @export
#'
#'
do_ORA_TERM2GENE <- function(proteins,
                             database,
                             pvalueCutoff = 0.05,
                             pAdjustMethod = "none",
                             qvalueCutoff = 0.2,
                             minGSSize = 3,
                             maxGSSize = 500) {

  # Prepare protein vectors
  sig.proteins <- names(proteins)[proteins == 1]

  background <- names(proteins)


  # Performing enrichment
  enrich.results <- clusterProfiler::enricher(gene = sig.proteins,
                                              universe = background,
                                              pvalueCutoff = pvalueCutoff,
                                              pAdjustMethod = pAdjustMethod,
                                              qvalueCutoff = qvalueCutoff,
                                              minGSSize = minGSSize,
                                              maxGSSize = maxGSSize,
                                              TERM2GENE = database,
                                              TERM2NAME = )

  # If enrichment failed
  if (is.null(enrich.results)) return(NULL)


  # Return
  return(invisible(enrich.results))

}
