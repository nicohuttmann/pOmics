#' Performs over-representation analysis using KEGG annotations
#'
#' @param proteins numeric/logical vector of proteins indicating group
#' @param pvalueCutoff p-value cutoff for annotations
#' @param pAdjustMethod one of "none","hochberg" (Benjamini-Hochberg correction), "bonferroni", "holm", "hommel", "BH", "BY", "fdr"
#' @param dataset dataset
#' @param view view results
#' @param return.all return enrichResult object; useful for further analysis of enrichment results
#' @param add.info add additional information to the results data frame
#'
#' @return
#' @export
#'
#'
do_ORA_KEGG <- function(proteins, pvalueCutoff = 0.05, pAdjustMethod = "none", dataset,
                        view = T, return.all = F, add.info = F) {

  dataset <- get_dataset(dataset)

  kegg_code <- get_dataset_attr(which = "kegg_code",
                                dataset = dataset)

  sig.proteins <- names(proteins)[proteins == 1]

  background <- names(proteins)



  kegg.results <- clusterProfiler::enrichKEGG(gene = sig.proteins,
                                              universe = background,
                                              organism = kegg_code,
                                              keyType = "uniprot",
                                              pAdjustMethod = pAdjustMethod,
                                              minGSSize = 10,
                                              pvalueCutoff = pvalueCutoff)


  results <- tibble::as_tibble(kegg.results@result) %>%
    dplyr::filter(pvalue < 0.05) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Proteins = paste(strsplit(geneID, split = "/")[[1]], collapse = ";"), .before = geneID) %>%
    dplyr::select(-c(geneID)) %>%
    dplyr::mutate(Genes = paste(p2g(strsplit(Proteins, split = ";")[[1]]), collapse = ";"), .before = Proteins)

  # View data frame immediately
  if (view) View(results)

  # Return
  if (!return.all) invisible(results)

  else return(list(results = results,
                   enrichResult = kegg.result))

}
