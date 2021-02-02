#' Calculates enrichment for each cluster level
#'
#' @param cor_list cor_list object
#' @param proteins annotated proteins or protein score vector
#' @param n max number of clusters
#' @param name name 
#'
#' @return
#' @export
#'
#'
enrich_dend_ <- function(cor_list, proteins, n = 10, name) {
  
  # Check input
  if(!hasArg(cor_list) || !hasArg(proteins)) stop("Missing input.")
  
  # name
  name <- ask_name(name = name, message = "Name for enrichment type: ")
  
  # Test anotation input; if numeric -> ks test
  if (is.numeric(proteins) && length(names(proteins)) > 0) {
    cor_list[[paste0("dend.enrich_", name)]] <- enrich_dend_ks(dend.table = cor_list[["dend.table"]],
                                                               protein.scores = proteins,
                                                               n = n)
  # Test anotation input; if protein vector -> fisher test   
  } else if(is.character(proteins)) {
    cor_list[[paste0("dend.enrich_", name)]] <- enrich_dend_fisher(dend.table = cor_list[["dend.table"]],
                                                                   annotated.proteins = proteins,
                                                                   n = n)
  # Incorrect input
  } else {
    stop("Incorrect input.")
  }
  
  
  # Return
  return(cor_list)
  
}
