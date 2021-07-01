#' Returns list filled with NULL of n length
#'
#' @param n length of list
#'
#' @return
#' @export
#'
#' 
nulllist <- function(n) {
  
  nl <- list(NULL,
             NULL,
             NULL,
             NULL,
             NULL,
             NULL,
             NULL,
             NULL,
             NULL,
             NULL,
             NULL,
             NULL,
             NULL,
             NULL,
             NULL,
             NULL,
             NULL,
             NULL,
             NULL,
             NULL)
  
  return(nl[1:n])
  
}
