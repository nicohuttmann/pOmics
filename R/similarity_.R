#' Adds similarity to cor_list
#'
#' @param cor_list cor_list object
#' @param method similarity method
#'
#' @return
#' @export
#'
#'
similarity_ <- function(cor_list, method = "none") {
  
  # Test cor_list
  if (!is.list(cor_list) || length(cor_list) == 0 || !"correlation" %in% names(cor_list)) stop("cor_list object seems corrupted.")
  
  # Add similarity 
  cor_list[["similarity"]] <- similarity(data = cor_list[["correlation"]],
                                         method = method)
  # Add attribute
  attr(cor_list, "similarity") <- method
  
  # Return list
  return(cor_list)
  
}
