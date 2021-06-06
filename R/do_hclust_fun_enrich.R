#' Title
#'
#' @param data_ 
#'
#' @return
#' @export
#'
#'
do_hclust_fun_enrich <- function(data_, max.k = 2, input = "dend_x") {
  
  # Check input
  if (!hasArg(data_)) {
    
    message("No data given.")
    
    invisible(NULL)
    
  }
  
  # Check if list or dataframe given
  list.input <- !is.data.frame(data_) & is.list(data_)
  
  # Check list input
  if (list.input & !input %in% names(data_)) {
    
    message("Data could not be found. Please specify correct <input>.")
    
    invisible(data_)
    
  }
  
  # Get data
  if (list.input) data <- data_[[input]]
  
  else data <- data_
  
  
  clusters <- cutree(data, k = 2) %>% 
    as.data.frame() %>% 
    data2tibble()
  
  cluster.list <- split(clusters[[1]], clusters[[2]])
  
  
  
  proteins <- cutree(data, k = 2)
  
  class(proteins) <- "character"
  
  a <- fun_enrich(proteins)
  
  
  merge_enrichment_results(a[["CC"]]) %>%
    simplify_enrichment_results() %>% 
    View()
  
  
  # Prepare return
  if (list.input) data_[[output]] <- data
  
  else data_ <- data
  
  # Return
  return(data_)
  
}