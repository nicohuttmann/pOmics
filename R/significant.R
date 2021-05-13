#' Returns logical vector indication significant p-values
#'
#' @param p.values named vector of p-values
#' @param pvalueCutoff limit for p.values and FDR for Benjamini-Hochberg-Correction
#' @param pAdjustMethod method to determine significant p-values ("none", "BH" (Benjamini-Hochberg-Correction)); see p.adjust.methods
#' @param na.value return value for NA
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
significant <- function(p.values, pvalueCutoff = 0.05, pAdjustMethod = "none", na.value = NA) {

  #
  p.values <- p.adjust(p = p.values, method = pAdjustMethod) < pvalueCutoff

  # Replace NAs with given entry
  p.values[is.na(p.values)] <- na.value

  # Return
  return(p.values)

}
