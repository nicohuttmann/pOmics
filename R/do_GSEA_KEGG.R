#' Performs gene set enrichment analysis using KEGG terms
#'
#' @param proteins numeric vector of proteins scores
#' @param pvalueCutoff p-value cutoff for annotations
#' @param pAdjustMethod one of "none","hochberg" (Benjamini-Hochberg correction), "bonferroni", "holm", "hommel", "BH", "BY", "fdr"
#' @param view view results
#' @param return.all return enrichResult object; useful for further analysis of enrichment results
#' @param add.info add additional information to the results data frame
#'
#' @return
#' @export
#'
#' 
do_GSEA_KEGG <- function(proteins, pvalueCutoff = 0.05, pAdjustMethod = "none", view = T, return.all = F, add.info = F) {
  
  organism_code <- clusterProfiler::search_kegg_organism(get_dataset_attr(which = "species", dataset = dataset),
                                                         by = "scientific_name")[1, 1]

  
  proteins.sorted <- sort(rank(proteins), decreasing = T)
  

  # Perform enrichment
  kegg.results <- clusterProfiler::gseKEGG(geneList      = proteins.sorted,
                                           organism      = organism_code,
                                           minGSSize     = 10,
                                           maxGSSize     = 120,
                                           pvalueCutoff  = pvalueCutoff,
                                           pAdjustMethod = pAdjustMethod,
                                           verbose       = FALSE,
                                           keyType       = "uniprot",
                                           scoreType     = "pos")
  
  
  

  results <- tibble::as_tibble(kegg.results@result) %>%
    dplyr::filter(pvalue < 0.05) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Proteins = paste(strsplit(core_enrichment, split = "/")[[1]], collapse = ";"), .before = core_enrichment) %>%
    dplyr::select(-c(core_enrichment, leading_edge, rank)) %>%
    dplyr::mutate(Genes = paste(p2g(strsplit(Proteins, split = ";")[[1]]), collapse = ";"), .before = Proteins)
  
  # View data frame immediately
  if (view) View(results)
  
  # Return
  if (!return.all) invisible(results)
  
  else return(list(results = results,
                   enrichResult = kegg.result))
  
  
  
  
}
