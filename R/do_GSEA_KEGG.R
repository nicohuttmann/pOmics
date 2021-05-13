#' Performs gene set enrichment analysis using KEGG terms
#'
#' @param proteins numeric vector of proteins scores
#' @param pvalueCutoff p-value cutoff for annotations
#' @param pAdjustMethod one of "none", "BH" (Benjamini-Hochberg correction), "hochberg", "bonferroni", "holm", "hommel", "BY", "fdr"
#' @param qvalueCutoff q-value cutoff for annotations
#' @param minGSSize minimum number of proteins for annotation to be used for enrichment
#' @param maxGSSize maximum number of proteins for annotation to be used for enrichment
#' @param dataset dataset
#' @param view view results
#' @param return.all return enrichResult object; useful for further analysis of enrichment results
#' @param add.info add additional information to the results data frame
#'
#' @return
#' @export
#'
#'
do_GSEA_KEGG <- function(proteins, pvalueCutoff = 0.05, pAdjustMethod = "none", qvalueCutoff = 0.2, minGSSize = 10,
                         maxGSSize = 120, dataset = dataset, view = T, return.all = F, add.info = F) {

  # Get dataset
  dataset <- get_dataset(dataset)

  # Kegg organism code
  kegg_code <- get_dataset_attr(which = "kegg_code",
                                dataset = dataset)

  # Prepare protein list
  proteins.sorted <- sort(rank(proteins), decreasing = T)


  # Perform enrichment
  kegg.results <- clusterProfiler::gseKEGG(geneList      = proteins.sorted,
                                           organism      = kegg_code,
                                           minGSSize     = minGSSize,
                                           maxGSSize     = maxGSSize,
                                           pvalueCutoff  = pvalueCutoff,
                                           pAdjustMethod = pAdjustMethod,
                                           verbose       = FALSE,
                                           keyType       = "uniprot",
                                           scoreType     = "pos")




  results <- tibble::as_tibble(kegg.results@result) %>%
    dplyr::filter(p.adjust < pvalueCutoff) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Proteins = paste(strsplit(core_enrichment, split = "/")[[1]], collapse = ";"), .before = core_enrichment) %>%
    dplyr::select(-c(core_enrichment, leading_edge, rank)) %>%
    dplyr::mutate(Genes = paste(p2g(strsplit(Proteins, split = ";")[[1]]), collapse = ";"), .before = Proteins)

  # View data frame immediately
  if (view) View(results)

  # Return
  if (!return.all) return(results)

  else return(list(results = results,
                   enrichResult = kegg.result))




}
