#' Performs overexpression enrichment on protein group vectors
#'
#' @param proteins character vector
#' @param database database to use
#'
#' @return
#' @export
#'
#' 
do_enrichment_mfisher <- function(proteins, database) {
  
  list.enrichment <- tibble::lst()
  
  # Check input
  for (group in unique(proteins)) {
    
    # 
    list.enrichment[[as.character(group)]] <- do_enrichment_fisher(proteins = ifelse(proteins == group, 1, 0),
                                                                   database = database)
  }
  
  # Return
  return(list.enrichment)
  
}
