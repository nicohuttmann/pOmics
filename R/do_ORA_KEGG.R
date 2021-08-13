#' Performs over-representation analysis using KEGG annotations
#'
#' @param proteins numeric/logical vector of proteins indicating group
#' @param pvalueCutoff p-value cutoff for annotations
#' @param pAdjustMethod one of "none", "BH" (Benjamini-Hochberg correction), "hochberg", "bonferroni", "holm", "hommel", "BY", "fdr"
#' @param qvalueCutoff q-value cutoff for annotations
#' @param minGSSize minimum number of proteins for annotation to be used for enrichment
#' @param maxGSSize maximum number of proteins for annotation to be used for enrichment
#' @param dataset dataset
#' @param view view results
#'
#' @return
#' @export
#'
#'
do_ORA_KEGG <- function(proteins,
                        pvalueCutoff = 0.05,
                        pAdjustMethod = "none",
                        qvalueCutoff = 0.2,
                        minGSSize = 10,
                        maxGSSize = 500,
                        dataset,
                        view = F) {

  # Get dataset
  dataset <- get_dataset(dataset)

  # Kegg organism code
  kegg_code <- get_dataset_attr(which = "kegg_code",
                                dataset = dataset)

  # Prepare protein vectors
  sig.proteins <- names(proteins)[proteins == 1]

  background <- names(proteins)


  # Performing enrichment
  kegg.results <- clusterProfiler::enrichKEGG(gene = sig.proteins,
                                              universe = background,
                                              organism = kegg_code,
                                              keyType = "uniprot",
                                              pvalueCutoff = pvalueCutoff,
                                              pAdjustMethod = pAdjustMethod,
                                              qvalueCutoff = qvalueCutoff,
                                              minGSSize = minGSSize,
                                              maxGSSize = maxGSSize)

  # If enrichment failed
  # if (is.null(kegg.results)) return(
  #   new("enrichResult",
  #       result = data.frame(ID = character(),
  #                           Description = character(),
  #                           GeneRatio = character(),
  #                           BgRatio = character(),
  #                           pvalue = double(),
  #                           p.adjust = double(),
  #                           qvalue = double(),
  #                           geneID = character(),
  #                           Count = double())))

  # # Prepare results data
  # results <- tibble::as_tibble(kegg.results@result) %>%
  #   dplyr::rowwise() %>%
  #   dplyr::mutate(Proteins = paste(strsplit(geneID, split = "/")[[1]], collapse = ";"), .before = geneID) %>%
  #   dplyr::select(-c(geneID)) %>%
  #   dplyr::mutate(Genes = paste(p2g(strsplit(Proteins, split = ";")[[1]]), collapse = ";"), .before = Proteins) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::filter(p.adjust < pvalueCutoff)

  # View data frame immediately
  if (view) View(kegg.results@result)

  # Return
  # if (!return.all)
  return(invisible(kegg.results))

  # else invisible(list(results = results,
  #                     enrichResult = kegg.results))

}
