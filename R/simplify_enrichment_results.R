#' Simplifies enrichment data frame by overlap between annotations, semantic similarity for GO terms and minimum protein count
#'
#' @param enrichment_df 
#' @param by.overlap use simplify_set_overlap
#' @param overlap.threshold minimum fraction of common proteins in protein set with lower p-value to be excluded
#' @param separator string separator of proteins
#' @param by.semantic use simplify_GO_terms_semantic
#' @param similarity.measure method to measure pairwise GO terms similarity (see ?GOSemSim::mgoSim)
#' @param similarity.threshold threshold to remove terms (0-1, default = 0.4)
#' @param dataset dataset 
#' @param minimum.count number of proteins per annotation
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>% 
#' 
simplify_enrichment_results <- function(enrichment_df, by.overlap = T, overlap.threshold = 0.7, separator = ";",
                                        by.semantic = F, similarity.measure = "Wang", similarity.threshold = 0.4, dataset, 
                                        overlap.first = T, minimum.count = 0) {
  
  
  if (by.overlap & overlap.first) {
    
    enrichment_df <- enrichment_df %>% 
      dplyr::filter(simplify_set_overlap(Proteins))
    
  }
  
  
  if (by.semantic) {
    
    enrichment_df <- enrichment_df %>% 
      dplyr::filter(simplify_GO_terms_semantic(ID))
    
  }
  
  if (by.overlap & !overlap.first) {
    
    enrichment_df <- enrichment_df %>% 
      dplyr::filter(simplify_set_overlap(Proteins))
    
  }
  
  # Apply minimum count
  enrichment_df <- enrichment_df %>% 
    dplyr::filter(Count >= minimum.count)
  
  
  
  # Return
  return(enrichment_df)
  
}
