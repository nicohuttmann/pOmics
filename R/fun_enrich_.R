#' Title
#'
#' @param cor_list 
#' @param name 
#' @param k 
#' @param l 
#' @param databases 
#'
#' @return
#' @export
#'
#'
fun_enrich_ <- function(cor_list, name, k, l, databases = "CC") {
  
  # if enrichment name given
  if (hasArg(name)) {
    
    if (!paste0("dend.enrich_", name) %in% names(cor_list)) stop("Name not found.")
    
    else fun_enrich(proteins = get_cluster_cut(cor_list = cor_list,
                                               k = cor_list[[paste0("dend.enrich_", name)]][["k"]]) == 
                      cor_list[[paste0("dend.enrich_", name)]][["l"]],
                    databases = databases,
                    view = TRUE,
                    return =  FALSE,
                    save = FALSE)
    
  } else if (hasArg(k)) {
    
    if (hasArg(l)) {
      
      fun_enrich(proteins = get_cluster_cut(cor_list = cor_list, k = k) == l,
                 databases = databases,
                 view = TRUE,
                 return =  FALSE,
                 save = FALSE)
      
    } else {
      
      proteins <- get_cluster_cut(cor_list = cor_list, k = k)
      mode(proteins) <- "character"
      fun_enrich(proteins = proteins,
                 databases = databases,
                 view = TRUE,
                 return =  FALSE,
                 save = FALSE)
      
    }
    
  }
  
}
