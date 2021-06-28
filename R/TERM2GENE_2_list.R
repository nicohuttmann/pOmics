#' Splits TERM2GENE dataframe to list
#'
#' @param TERM2GENE TERM2GENE dataframe
#'
#' @return
#' @export
#'
#'
TERM2GENE_2_list <- function(TERM2GENE) {

  return(split(x = TERM2GENE$GENE, TERM2GENE$TERM))

}
