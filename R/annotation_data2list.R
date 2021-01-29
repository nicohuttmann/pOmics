#' Transforms data frame to annotation list
#'
#' @param data.annotation annotation data from UniProt.ws
#' @param sep separator
#'
#' @return
#' @export
#'
#'
annotation_data2list <- function(data.annotation, sep = "; ") {
  
  annotation.list <- list()
  
  # 
  for (i in 1:nrow(data.annotation)) {
    if (is.na(data.annotation[i, 2])) {
      annotation.list[data.annotation[i, 1]] <- list(NULL)
    } else {
      annotation.list[[data.annotation[i, 1]]] <- unlist(strsplit(data.annotation[i, 2], split = sep))
    }
  }
  
  # 
  return(annotation.list)
  
}
