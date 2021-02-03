#'  Add connectivity to cor_list
#'
#' @param cor_list cor_list object
#' @param scale Scale connectivity values
#' @param plot plot results 
#' @param fun.enrich do functional enrichment on connectivity values
#'
#' @return
#' @export
#'
#'
connectivity_ <- function(cor_list, scale = T, plot = T, fun.enrich) {
  
  # 
  con <- connectivity(adjacency.matrix = cor_list[["adjacency"]], scale = T)
  
  cor_list[["connectivity"]] <- con
  
  # Plot
  if (plot) plot(con, 20)
  
  # Functional enrichment
  if (hasArg(fun.enrich)) fun_enrich(proteins = con, databases = fun.enrich)
  
  # Return list
  return(cor_list)
  
}