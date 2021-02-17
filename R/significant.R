#' Returns logical vector indication significant p-values
#'
#' @param p.values named vector of p-values
#' @param method method to determine significant p-values ("threshold", "bhc" (Benjamini-Hochberg-Correction))
#' @param threshold threshold for p.values and FDR for Benjamini-Hochberg-Correction
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
significant <- function(p.values, method = "threshold", threshold = 0.05, na = F) {

  # Threshold
  if (method == "threshold") {

    # Apply threshold
    p.values <- p.values < threshold

  # Benjamini-Hochberg method
  } else if (method == "bhc") {

    p.values <- bhc(p.values = p.values, FDR = threshold) %>%
      dplyr::pull(var = "significant", name = "variables")

  }

  # Replace NAs with given entry
  p.values[is.na(p.values)] <- na

  # Return
  return(p.values)

}
