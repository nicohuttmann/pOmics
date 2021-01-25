#' Calculates Fisher's exact test and returns p-value
#'
#' @param a logical vector
#' @param b logical vector
#'
#' @return
#' @export
#'
#'
fisher_pvalue <- function(a, b) {
  
  if (length(names(b)) == 0 || !identical(names(a), names(b))) {
    c <- a
    c[] <- F
    c[names(b)] <- T
    b <- c
  }
  # 
  return(fisher.test(table(a, b), alternative = "greater")$p.value)
  
}
