#' Removes similar protein sets based on minimum overlap
#'
#' @param proteins vector containing protein strings separated by ;
#' @param overlap.threshold minimum fraction of common proteins in protein set with lower p-value to be excluded
#' @param separator string separator of proteins
#'
#' @return Names logical vector
#' @export
#'
#' 
simplify_set_overlap <- function(proteins, overlap.threshold = 0.7, separator = ";") {
  
  
  proteins.test <- proteins
  
  names(proteins.test) <- seq_along(proteins.test)
  
  proteins.keep <- c()
  
  # 
  while (length(proteins.test) > 0) {
    
    # First protein of list
    protein <- proteins.test[1]
    
    # Keep first protein of current list
    proteins.keep <- c(proteins.keep, names(protein)[1])
    
    # Remove protein from testing vector
    proteins.test <- proteins.test[-1]
    
    # Check if any proteins left to test
    if (length(proteins.test) > 0) {
      
      protein1 <- strsplit_(protein, separator)
      
      # Remove similar proteins
      proteins.test <- proteins.test[sapply(X = proteins.test, 
                                            FUN = function(x) 
                                              {length(intersect(protein1, strsplit_(x, separator))) / length(strsplit_(x, separator)) < overlap.threshold})]
      
    }
    
  }
  
  return <- rep(FALSE, length(proteins))
  
  names(return) <- proteins
  
  return[as.numeric(proteins.keep)] <- TRUE
  
  # Return
  return(return)
  
}

