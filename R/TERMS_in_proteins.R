#' Returns logical dataframe which proteins is annotated by given terms
#'
#' @param proteins 
#' @param TERMS 
#' @param TERM2GENE 
#'
#' @return
#' @export
#'
#' 
TERMS_in_proteins <- function(proteins, TERMS, TERM2GENE) {
  
  
  TERM_data <- dplyr::tibble(TERM = TERMS)
  
  
  for (i in proteins) {
    
    TERM_data <- TERM_data %>% 
      dplyr::mutate(!!i := TERM %in% dplyr::filter(TERM2GENE, GENE == !!i)$TERM)
    
  }
  
  # Return
  return(TERM_data)
  
}
