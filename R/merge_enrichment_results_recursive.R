#' merges a nested list of enrichment results
#'
#' @param enrichment_list nested list of enrichment dataframes
#'
#' @return
#' @export
#'
#' 
merge_enrichment_results_recursive <- function(enrichment_list) {
  
  
  # Keep iterating if not fully merged
  while (!is.data.frame(enrichment_list)) {
    
    
    if (is.data.frame(enrichment_list[[1]])) {
      
      enrichment_list <- merge_enrichment_results(enrichment_list)
      
      
    } else {
      
      
      for (i in seq_along(enrichment_list)) {
        
        enrichment_list[[i]] <- merge_enrichment_results_recursive(enrichment_list[[i]])
        
      }
      
    }
    
  }
  
  
  return(enrichment_list)
  
  
}
